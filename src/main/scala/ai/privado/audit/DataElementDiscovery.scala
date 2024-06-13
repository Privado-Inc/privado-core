package ai.privado.audit

import ai.privado.cache.TaggerCache
import ai.privado.dataflow.Dataflow
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import ai.privado.model.Language
import ai.privado.model.Language.Language
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DataElementDiscoveryUtils {

  private val logger = LoggerFactory.getLogger(getClass)
  // Not used for security-purposes. Only to generate a unique identifier.
  private lazy val md5 = java.security.MessageDigest.getInstance("MD5")

  // Regular expression pattern to filter common language types
  private val filterCommonLangTypes =
    "(?i)(class|window|str|list|dict|bool|chr|icon|boolean|number|bigdecimal|let|define|enum|asttype|nil|null|none|java|undefined|nan|empty|objectid|_id|tostring|valueof|json|mime|true|false|before|after|arr|typeof|match|to_dict|toarray|todate|case|<global>|<fakeNew>|(buf|array|int|num|type|float|byte|string|blob).{0,1}|set|map|hashmap|vector|deque|function|method|property|char|short|long|double|decimal|datetime|date|time|timestamp|enum|flag|void|interface|trait|package|namespace|record|tuple|struct|component|hook|hoc|service)"
  // Regular expression pattern to filter common variable names
  private val filterCommonVars =
    "(?i)(cls|self|ctx|constructor|prototype|main|use|foreach|copy|skip|replace|slice|fill|some|every|concat|contains|apply|merge|stmt|format|name|data|regexp|.{0,1}(sort|in|slug|match|ne|regex|or|sum|and)|session|status|event|env|cmd|push|join|split|splice|filter|reduce|shift|unshift|retry|start|buffer|thread|length|staticmethod|app|next|end|req|console|push|pop|handler|server|catch|then|uri|split|exp|other|info|debug|warning|critical|exception|size|max|text|http|query|href|write|(sql|row|len|err|res|ret|obj|msg|val|key|item|url|tmp|col|file|img|test|result|path|module|import|export|log|key|value|include|load|dump).{0,1})"

  // List of prefixes to filter out common variables that start with these values
  private val filterCommonVarsStartsWith =
    "$obj|$group|$set|__|_tmp_|tmp|$iterLocal|_result|file|is|sha_|this|get|set|post|put|update|create|clear|check|find|insert|assert|parse|generate|validate|process|download|upload|delete|handle|param|attr|arg|_iterator|{|log|error|iterator|logger|<tmp-|iter_tmp|toLocale|indexOf"
  private val filterCommonVarsStartsWithArr = filterCommonVarsStartsWith.split("\\|")

  // Additional Language specific filters
  def getLanguageSpecificFilters(lang: Language): String = {
    lang match {
      case Language.JAVASCRIPT =>
        "(?i)(i18n|ajv|axios|require|express|moment|document|hasOwnProperty|date|usestate|useeffect|dispatch|ngif|ngforof|inject|component|router|validators|formgroup|formcontrol|subscribe|observable|jquery|vue|react|angular)"
      case Language.PYTHON =>
        "(?i)(print|boto.{0,1}|s3|requests|append|extend|list_objects.{0,6}|pytest|datetime|pandas|numpy|scipy|sklearn|matplotlib|flask|django|argparse|os|sys)"
      case Language.JAVA | Language.KOTLIN =>
        "(?i)(system|out|buf|(com|io|org|net|androidx|software|io)[.]{1}.*|printstream|scanner|stringbuilder|thread|exception|bufferedreader|inputstream|outputstream|integer|character)"
      case Language.GO =>
        "(?i)(context|block|nonce|syscall|buf|fmt|http|log|os|io|error|http|httpclient|httprequest|httpresponse)"
      case Language.PHP =>
        "(?i)(_post|_get|_session|_files|_server|_cookie|uid|mysqli|pdo|print_r|var_dump|echo|header|session_start|session_destroy)"
      case Language.RUBY =>
        "(?i)(node|tree|raise|object|gem|rails|sinatra|rake|erb|haml)"
      case Language.CSHARP =>
        "(?i)(table|migrationBuilder|modelBuilder|program|yaml|context|cts|instance|idx|system|console|io|collections|generic|threading|tasks|linq|xml|entity|configuration|component|service)"
    }
  }

  def nodeIdentifier(filePath: String, name: String, nodeType: String, lineNumber: String): String =
    md5
      .digest(s"$filePath-$name".getBytes)
      .map(0xff & _)
      .map("%02x".format(_))
      .foldLeft("")(_ + _)

  def nodeOffset(node: TypeDecl | Member | MethodParameterIn | Identifier | Local | FieldIdentifier): String =
    node.lineNumber match {
      case Some(offset) => offset.toString
      case _            => ""
    }

  def getSourceRuleId(node: TypeDecl| Member | MethodParameterIn | Identifier | Local | FieldIdentifier): String = {
    // Extract the source rule ID, defaulting to AUDIT_EMPTY_CELL_VALUE if not found
    val sourceRuleId = node.tag.nameExact(Constants.id).value.headOption.getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE)

    // Determine if tagging is disabled by DED
    val taggingDisabledByDED = node.tag.nameExact(InternalTag.TAGGING_DISABLED_BY_DED.toString).nonEmpty

    if (taggingDisabledByDED)  {
      AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
    } else {
      sourceRuleId
    }
  }

  def getNodeLocationAndUniqueId(
    node: TypeDecl | Member | MethodParameterIn | Identifier | Local | FieldIdentifier,
    name: String,
    nodeType: String
  ): (String, String, String) = {
    val path         = node.file.name.headOption.getOrElse(Constants.EMPTY)
    val lineNumber   = DataElementDiscoveryUtils.nodeOffset(node)
    val nodeUniqueId = DataElementDiscoveryUtils.nodeIdentifier(path, name, nodeType, lineNumber)

    (path, lineNumber, nodeUniqueId)
  }

  // Add Identifiers in workbookResult
  def getIdentifiers(
    xtocpg: Try[Cpg],
    workbookResult: ListBuffer[List[String]],
    lang: Language
  ): ListBuffer[List[String]] = {
    val identifiers = xtocpg match {
      case Success(cpg) => {
        cpg.identifier
          // Filter out identifiers with length <= 2
          .filter(i => i.name.length > 2)
          // Filter out identifiers that start with any of the specified prefixes
          .filter(i => !filterCommonVarsStartsWithArr.exists(xx => i.name.startsWith(xx)))
          // Filter out identifiers matching common language types pattern
          .filter(i => !i.name.matches(filterCommonLangTypes))
          // Filter out identifiers matching common variable names pattern
          .filter(i => !i.name.matches(filterCommonVars))
          // Filter out identifiers matching language-specific filters
          .filter(i => !i.name.matches(DataElementDiscoveryUtils.getLanguageSpecificFilters(lang)))
          // Filter out identifier which are potentially function calls
          .filter(i => !i.typeFullName.matches("\\(.*\\).=>.*"))
          .l
      }
      case Failure(ex) => {
        logger.debug(f"Error while getting Identifier ", ex)
        List[Identifier]()
      }
    }

    val addedIdentifiers = mutable.Set[String]()
    identifiers.foreach(identifier => {
      val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_IDENTIFIER
      val (path, lineNumber, nodeUniqueId) =
        DataElementDiscoveryUtils.getNodeLocationAndUniqueId(identifier, identifier.name, nodeType)

      val identifierUniqueKey = s"${identifier.typeFullName}$path${identifier.name}"
      val sourceRuleId = DataElementDiscoveryUtils.getSourceRuleId(identifier)

      if (
        identifier.name.nonEmpty && !identifier.name
          .matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX)
        && !identifier.name
          .matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX)
        && !identifier.name.matches(AuditReportConstants.JS_ELEMENTS_TO_BE_EXCLUDED)
        && !addedIdentifiers.contains(identifierUniqueKey)
      )
        addedIdentifiers.add(identifierUniqueKey)
      workbookResult += List(
        identifier.typeFullName,
        identifier.file.name.headOption.getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE),
        "0.0",
        identifier.name,
        identifier.typeFullName,
        sourceRuleId.startsWith("Data.Sensitive.").toString,
        sourceRuleId,
        AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        lineNumber,
        nodeUniqueId,
        nodeType
      )
    })

    workbookResult
  }

  // Add FieldIdentifiers in workbookResult
  def getFieldAccessIdentifier(
    xtocpg: Try[Cpg],
    workbookResult: ListBuffer[List[String]],
    lang: Language
  ): ListBuffer[List[String]] = {
    val fieldIdentifiers = xtocpg match {
      case Success(cpg) => {
        cpg.fieldAccess.fieldIdentifier
          // Filter out identifiers with length <= 2
          .filter(i => i.canonicalName.length > 2)
          // Filter out identifiers that start with any of the specified prefixes
          .filter(i => !filterCommonVarsStartsWithArr.exists(xx => i.canonicalName.startsWith(xx)))
          // Filter out identifiers matching common language types pattern
          .filter(i => !i.canonicalName.matches(filterCommonLangTypes))
          // Filter out identifiers matching common variable names pattern
          .filter(i => !i.canonicalName.matches(filterCommonVars))
          // Filter out identifiers matching language-specific filters
          .filter(i => !i.canonicalName.matches(DataElementDiscoveryUtils.getLanguageSpecificFilters(lang)))
          // Filter out fieldIdentifiers like `session` of format `res.session.XXX`
          .whereNot(_.astParent.astParent.isCall.name("<operator>.fieldAccess"))
          .l
      }
      case Failure(ex) => {
        logger.debug(f"Error while getting FieldIdentifier ", ex)
        List[FieldIdentifier]()
      }
    }

    val addedFieldIdentifiers = mutable.Set[String]()
    fieldIdentifiers.foreach(idenfier => {
      val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_FIELD_IDENTIFIER
      val (path, lineNumber, nodeUniqueId) =
        DataElementDiscoveryUtils.getNodeLocationAndUniqueId(idenfier, idenfier.canonicalName, nodeType)
      val localsUniqueKey = s"NA$path${idenfier.canonicalName}"
      val sourceRuleId = DataElementDiscoveryUtils.getSourceRuleId(idenfier)

      if (
        idenfier.canonicalName.nonEmpty && !idenfier.canonicalName
          .matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX) && !idenfier.canonicalName
          .matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX)
        && !idenfier.canonicalName.matches(AuditReportConstants.JS_ELEMENTS_TO_BE_EXCLUDED)
        && !addedFieldIdentifiers.contains(localsUniqueKey)
      )
        addedFieldIdentifiers.add(localsUniqueKey)
      workbookResult += List(
        "NA",
        idenfier.file.head.name,
        "0.0",
        idenfier.canonicalName,
        "NA",
        sourceRuleId.startsWith("Data.Sensitive.").toString,
        sourceRuleId,
        AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        lineNumber,
        nodeUniqueId,
        nodeType
      )
    })

    workbookResult
  }

  // Add Members in workbookResult for given className set
  def getMemberUsingClassName(
    xtocpg: Try[Cpg],
    classNameSet: Set[String],
    lang: Language
  ): Map[TypeDecl, List[Member]] = {
    logger.info("Process Member Name from cpg")
    val memberInfoMap = mutable.HashMap[TypeDecl, List[Member]]()

    xtocpg match {
      case Success(cpg) => {
        classNameSet.foreach(className => {
          try {
            cpg.typeDecl
              .where(_.fullName(className))
              .foreach(typeDeclNode => {
                if (typeDeclNode.member.nonEmpty) {
                  val members =
                    typeDeclNode.member
                      // Filter out members with length <= 2
                      .filter(i => i.name.length > 2)
                      // Filter out members that start with any of the specified prefixes
                      .filter(i => !filterCommonVarsStartsWithArr.exists(xx => i.name.startsWith(xx)))
                      // Filter out members matching common language types pattern
                      .filter(i => !i.name.matches(filterCommonLangTypes))
                      // Filter out members matching common variable names pattern
                      .filter(i => !i.name.matches(filterCommonVars))
                      // Filter out members matching language-specific filters
                      .filter(i => !i.name.matches(DataElementDiscoveryUtils.getLanguageSpecificFilters(lang)))
                      .dedup
                      .l
                  memberInfoMap.put(typeDeclNode, members)
                }
              })
          } catch {
            case ex: Exception =>
              logger.debug(f"Skipping the class ${className} due to invalid regex issue", ex)
          }
        })
      }
      case Failure(exception) => {
        logger.debug("Failed to process member name from cpg", exception)
      }
    }
    logger.info("Successfully Processed member name from cpg")
    memberInfoMap.toMap
  }

  // Add logic to filter out the unwanted entries from audit Sources
  def filterEntriesWithEmptyMemberName(auditData: List[List[String]]): List[List[String]] = {
    auditData.filter { data =>
      val memberName: String = data(3)
      memberName.nonEmpty
    }
  }

  def getSourceUsingRules(xtocpg: Try[Cpg]): List[String] = {
    logger.info("Process Class Name from cpg")
    val classNameList = ListBuffer[String]()
    xtocpg match {
      case Success(cpg) => {
        val typeDeclList = cpg.typeDecl
          .filter(_.order > 0)
          .toList

        typeDeclList.foreach(node => {
          if (node.fullName.nonEmpty) {
            classNameList += node.fullName
          }
        })
      }
      case Failure(exception) => {
        logger.debug("Failed to process class name from cpg", exception)
      }
    }
    logger.info("Successfully Processed Class Name from cpg")
    classNameList.toList
  }

  def getMethodParametersFromTypes(
    xtocpg: Try[Cpg],
    classNameRuleList: Set[String],
    exludeParamaRegex: String
  ): Map[TypeDecl, ListBuffer[MethodParameterIn]] = {
    val methodParameterMap = new mutable.HashMap[TypeDecl, ListBuffer[MethodParameterIn]]()
    xtocpg match {
      case Success(cpg) => {
        classNameRuleList.foreach(className => {
          try {
            cpg.typeDecl.where(_.fullName(className)).l.headOption match {
              case Some(typeDecl) =>
                cpg.method
                  .fullName(typeDecl.fullName)
                  .foreach(method => {
                    if (!methodParameterMap.contains(typeDecl)) {
                      methodParameterMap.put(typeDecl, new ListBuffer[MethodParameterIn])
                    }
                    val methodParameterInfo = methodParameterMap(typeDecl)
                    method.parameter.l
                      .whereNot(_.name(exludeParamaRegex))
                      .foreach(parameter => {
                        methodParameterInfo += parameter
                      })
                    methodParameterMap.put(typeDecl, methodParameterInfo)
                  })
              case None =>
                logger.debug("head of empty list")
            }
          } catch {
            case ex: Exception =>
              logger.debug(f"Skipping the class ${className} due to invalid regex issue", ex)
          }
        })
      }
      case Failure(exception) => {
        logger.debug("Failed to process method parameter info from cpg", exception)
      }
    }
    methodParameterMap.toMap
  }

  def getHeaderList(): List[String] = {
    List(
      AuditReportConstants.ELEMENT_DISCOVERY_CLASS_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_FILE_NAME,
      AuditReportConstants.FILE_PRIORITY_SCORE,
      AuditReportConstants.ELEMENT_DISCOVERY_MEMBER_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_MEMBER_TYPE,
      AuditReportConstants.ELEMENT_DISCOVERY_TAGGED_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_SOURCE_RULE_ID,
      AuditReportConstants.ELEMENT_DISCOVERY_INPUT_COLLECTION,
      AuditReportConstants.ELEMENT_DISCOVERY_COLLECTION_ENDPOINT,
      AuditReportConstants.ELEMENT_DISCOVERY_METHOD_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_SOURCE_LINE_NUMBER,
      AuditReportConstants.ELEMENT_DISCOVERY_VARIABLE_ID,
      AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE
    )
  }

  def updateWorkbookResultsToGetUniqueSourcePerFile(workbookResult: List[List[String]]): List[List[String]] = {
    try {
      println(workbookResult.size)
      // Group entries by filename
      val groupedByFilename = workbookResult.groupBy(entry => entry(1))

      // For each group, filter unique items based on name
      val filteredResults = groupedByFilename.values.flatMap { entries =>
        val uniqueByName = entries.groupBy(entry => entry(3)).values.map(_.head)
        uniqueByName
      }.toList
      println("Done filter results")
      println(filteredResults.size)
      DataElementDiscoveryUtils.filterEntriesWithEmptyMemberName(filteredResults)
    } catch {
      case e: Exception =>
        println(s"An error occurred while filtering the workbook result: ${e.getMessage}")
        workbookResult
    }
  }
}

object DataElementDiscoveryJava {

  private val logger = LoggerFactory.getLogger(getClass)

  // Get list of Class Name having getter and setter method
  def getSourceUsingRules(xtocpg: Try[Cpg]): List[String] = {
    logger.info("Process Class Name from cpg")
    val classNameList = ListBuffer[String]()
    xtocpg match {
      case Success(cpg) => {
        // Get DTO/Entity Class name
        val typeDeclList = cpg.typeDecl
          .filter(_.order > 0)
          .whereNot(_.name(AuditReportConstants.ELEMENT_DISCOVERY_EXCLUDE_CLASS_NAME_REGEX))
          .or(
            _.where(_.method.name(AuditReportConstants.ELEMENT_DISCOVERY_GET_SET_METHOD_REGEX)),
            _.where(_.method.name(AuditReportConstants.ELEMENT_DISCOVERY_OVERRIDE_METHOD_REGEX)),
            _.where(_.annotation.name(AuditReportConstants.ELEMENT_DISCOVERY_GETTER_SETTER_REGEX))
          )
          .toList
        typeDeclList.foreach(node => {
          if (node.fullName.nonEmpty) {
            classNameList += node.fullName
          }
        })
      }
      case Failure(exception) => {
        logger.debug("Failed to process class name from cpg", exception)
      }
    }
    logger.info("Successfully Processed Class Name from cpg")
    classNameList.toList
  }

  // Search other potential Class in package
  def extractClassFromPackage(xtocpg: Try[Cpg], classFullNameList: Set[String]): Set[String] = {
    val packageNameSet = mutable.Set[String]()

    classFullNameList.foreach(fullName => {
      val lastDotIndex = fullName.lastIndexOf('.')
      if (lastDotIndex > 0) {
        val packageName = fullName.substring(0, lastDotIndex)
        packageNameSet += packageName
      }
    })

    val derivedClassName = ListBuffer[String]()
    xtocpg match {
      case Success(cpg) => {
        packageNameSet.foreach(packageName => {
          val pattern = s"$packageName.*"
          val typeDeclList = cpg.typeDecl
            .filter(_.order > 0)
            .where(_.fullName(pattern))
            .whereNot(_.name(AuditReportConstants.ELEMENT_DISCOVERY_EXCLUDE_CLASS_NAME_REGEX))
            .toList
          typeDeclList.foreach(typeDecl => derivedClassName += typeDecl.fullName)
        })
      }
      case Failure(exception) => {
        logger.debug("Failed to Extract package classes from cpg", exception)
      }
    }
    derivedClassName.toSet
  }

  // Get Collection Input Class Name
  def getCollectionInputList(xtocpg: Try[Cpg]): List[String] = {
    val collectionInputList = ListBuffer[String]()

    xtocpg match {
      case Success(cpg) => {
        // Get tagged collection input list
        val parameterList = cpg.parameter
          .where(_.method.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
          .whereNot(_.typeFullName(AuditReportConstants.AUDIT_BUILT_IN_CLASS_REGEX))
          .whereNot(_.typeFullName(AuditReportConstants.ELEMENT_DISCOVERY_EXCLUDE_CLASS_NAME_REGEX))
          .l
        parameterList.foreach(parameter => {
          collectionInputList += parameter.typeFullName
        })
      }
      case Failure(exception) => {
        logger.debug("Failed to process method parameter from cpg", exception)
        logger.debug("exception: ", exception.printStackTrace())
      }
    }
    collectionInputList.toList
  }

  def getCollectionMethodInfo(xtocpg: Try[Cpg]): Map[String, ListBuffer[CollectionMethodInfo]] = {
    // className -> (MethodName, Endpoint)
    val collectionMethodInfoMap = new mutable.HashMap[String, ListBuffer[CollectionMethodInfo]]()
    xtocpg match {
      case Success(cpg) => {
        val parameterList = cpg.parameter
          .where(_.method.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name))
          .whereNot(_.typeFullName(AuditReportConstants.AUDIT_BUILT_IN_CLASS_REGEX))
          .whereNot(_.typeFullName(AuditReportConstants.ELEMENT_DISCOVERY_EXCLUDE_CLASS_NAME_REGEX))
          .l
        parameterList.foreach(parameter => {

          // Get complete Endpoint path
          val endpointTag = parameter.method.tag.where(_.name(InternalTag.COLLECTION_METHOD_ENDPOINT.toString)).head
          // Append endpoint and method name when Method parameter having same type
          if (!collectionMethodInfoMap.contains(parameter.typeFullName)) {
            collectionMethodInfoMap
              .put(parameter.typeFullName, new ListBuffer[CollectionMethodInfo])
          }

          val collectionMethodInfo = collectionMethodInfoMap(parameter.typeFullName)
          collectionMethodInfo += CollectionMethodInfo(parameter.method.code, endpointTag.value)
        })
      }
      case Failure(exception) => {
        logger.debug("Failed to process collection method info from cpg", exception)
        logger.debug("exception: ", exception.printStackTrace())
      }
    }
    collectionMethodInfoMap.toMap
  }

  def getFileScore(absoluteFileName: String, xtocpg: Try[Cpg]): String = {
    var score = 0.0
    xtocpg match {
      case Success(cpg) => {
        if (Dataflow.getSources(cpg).file.where(_.name(absoluteFileName)).length > 0) {
          score += 1
        }
        val objectDefinationDirectoryPattern = "(?i)pojo|model|dao|mapper|entity".r

        objectDefinationDirectoryPattern.findFirstMatchIn(absoluteFileName) match {
          case Some(_) => score += 0.25
          case None    =>
        }

        val fileName = absoluteFileName.substring(absoluteFileName.lastIndexOf("/") + 1)

        val datasubjectPattern = "(?i)person|user|customer".r

        datasubjectPattern.findFirstMatchIn(fileName) match {
          case Some(_) => score += 1
          case None    =>
        }
        score.toString
      }
      case Failure(exception) => {
        logger.debug("Failed to calculate file score", exception)
        logger.debug("exception: ", exception.printStackTrace())
        "0"
      }
    }
  }

  def processDataElementDiscovery(
    xtocpg: Try[Cpg],
    taggerCache: TaggerCache,
    lang: Language = Language.JAVA
  ): List[List[String]] = {
    logger.info("Initiated the audit engine")
    val classNameRuleList    = getSourceUsingRules(xtocpg)
    val collectionInputList  = getCollectionInputList(xtocpg)
    val collectionMethodInfo = getCollectionMethodInfo(xtocpg)
    val derivedClassName     = extractClassFromPackage(xtocpg, (classNameRuleList ++ collectionInputList).toSet)
    val memberInfo = DataElementDiscoveryUtils.getMemberUsingClassName(
      xtocpg,
      (classNameRuleList ++ collectionInputList ++ derivedClassName).toSet,
      lang
    )
    var workbookResult      = new ListBuffer[List[String]]()
    val typeDeclMemberCache = taggerCache.typeDeclMemberCache

    // Stores ClassName --> (MemberName --> SourceRuleID)
    val taggedMemberInfo = mutable.HashMap[String, mutable.HashMap[String, String]]()

    // Reverse the mapping to MemberName --> sourceRuleId
    typeDeclMemberCache.foreach { case (key, value) =>
      val reverseMap = mutable.HashMap[String, String]()
      value.foreach { case (ruleName, memberSet) =>
        memberSet.foreach(member => {
          reverseMap.put(member.name, ruleName)
        })
      }
      taggedMemberInfo.put(key, reverseMap)
    }

    // Construct the excel sheet and fill the data
    try {
      memberInfo.foreach {
        case (key, value) => {
          val isCollectionInput = if (collectionInputList.contains(key.fullName)) "YES" else "NO"
          if (taggedMemberInfo.contains(key.fullName)) {
            val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_MEMBER
            val (path, lineNumber, nodeUniqueId) =
              DataElementDiscoveryUtils.getNodeLocationAndUniqueId(key, key.fullName, nodeType)

            if (collectionMethodInfo.contains(key.fullName)) {
              collectionMethodInfo(key.fullName).foreach(info => {
                workbookResult += List(
                  key.fullName,
                  key.file.head.name,
                  getFileScore(path, xtocpg),
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_CHECKED_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  isCollectionInput,
                  info.endpoint,
                  info.methodDetail,
                  lineNumber,
                  nodeUniqueId,
                  nodeType
                )
              })
            } else {
              workbookResult += List(
                key.fullName,
                key.file.head.name,
                getFileScore(path, xtocpg),
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_CHECKED_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                isCollectionInput,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                nodeUniqueId,
                lineNumber,
                nodeType
              )
            }
            val ruleMemberInfo = taggedMemberInfo.getOrElse(key.fullName, new mutable.HashMap[String, String])
            value.foreach {
              case (member: Member) => {
                val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_MEMBER
                val (_, lineNumber, nodeUniqueId) =
                  DataElementDiscoveryUtils.getNodeLocationAndUniqueId(member, member.name, nodeType)

                if (ruleMemberInfo.contains(member.name)) {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    getFileScore(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                    member.name,
                    member.typeFullName,
                    AuditReportConstants.AUDIT_CHECKED_VALUE,
                    ruleMemberInfo.getOrElse(member.name, ""),
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    lineNumber,
                    nodeUniqueId,
                    nodeType
                  )
                } else {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    getFileScore(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                    member.name,
                    member.typeFullName,
                    AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    lineNumber,
                    nodeUniqueId,
                    nodeType
                  )
                }
              }
            }
          } else {
            val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_MEMBER
            val (path, lineNumber, nodeUniqueId) =
              DataElementDiscoveryUtils.getNodeLocationAndUniqueId(key, key.fullName, nodeType)

            if (collectionMethodInfo.contains(key.fullName)) {
              collectionMethodInfo(key.fullName).foreach(info => {
                workbookResult += List(
                  key.fullName,
                  key.file.head.name,
                  getFileScore(path, xtocpg),
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  isCollectionInput,
                  info.endpoint,
                  info.methodDetail,
                  lineNumber,
                  nodeUniqueId,
                  nodeType
                )
              })
            } else {
              workbookResult += List(
                key.fullName,
                key.file.head.name,
                getFileScore(path, xtocpg),
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                isCollectionInput,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                lineNumber,
                nodeUniqueId,
                nodeType
              )
            }
            value.foreach {
              case (member: Member) => {
                val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_MEMBER
                val (_, lineNumber, nodeUniqueId) =
                  DataElementDiscoveryUtils.getNodeLocationAndUniqueId(member, member.name, nodeType)

                workbookResult += List(
                  key.fullName,
                  key.file.head.name,
                  getFileScore(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                  member.name,
                  member.typeFullName,
                  AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  lineNumber,
                  nodeUniqueId,
                  nodeType
                )
              }
            }
          }
        }
      }

      // Adding Identifiers
      workbookResult = DataElementDiscoveryUtils.getIdentifiers(xtocpg, workbookResult, lang)

      logger.info("Shutting down audit engine")
    } catch {
      case ex: Exception =>
        logger.debug("Failed to process Data Element Discovery report", ex)
    }
    DataElementDiscoveryUtils.getHeaderList() +: DataElementDiscoveryUtils.updateWorkbookResultsToGetUniqueSourcePerFile(workbookResult.toList)
  }

  case class CollectionMethodInfo(var methodDetail: String, var endpoint: String)

}

object DataElementDiscovery {
  private val logger = LoggerFactory.getLogger(getClass)

  def getFileScoreJS(absoluteFileName: String, xtocpg: Try[Cpg]): String = {
    var score = 0.0
    xtocpg match {
      case Success(cpg) => {
        if (Dataflow.getSources(cpg).file.where(_.name(absoluteFileName)).length > 0) {
          score += 1
        }
        val probableSourcesDirectoryPattern = "(?i)models?|services?|controllers?|stores?".r
        probableSourcesDirectoryPattern.findFirstMatchIn(absoluteFileName) match {
          case Some(_) => score += 0.5
          case None    =>
        }

        val fileName = absoluteFileName.substring(absoluteFileName.lastIndexOf("/") + 1)

        val dataSubjectPattern = "(?i)person|users?|customer".r
        dataSubjectPattern.findFirstMatchIn(fileName) match {
          case Some(_) => score += 0.5
          case None    =>
        }

        val sourceFiles = "(?i)register|store|login".r
        sourceFiles.findFirstMatchIn(fileName) match {
          case Some(_) => score += 0.5
          case None    =>
        }

        val lowPriorityFolders = "(?i)routes?|test|public|assets?|static|spec|configs?".r
        lowPriorityFolders.findFirstMatchIn(absoluteFileName) match {
          case Some(_) => score = 0.0
          case None    =>
        }
        score.toString
      }
      case Failure(exception) => {
        val fileName = absoluteFileName.substring(absoluteFileName.lastIndexOf("/") + 1)
        logger.debug(s"Failed to calculate file priority score for '$fileName'")
        logger.debug("Failed to calculate file score", exception)
        logger.debug("exception: ", exception.printStackTrace())
        "0"
      }
    }
  }

  def processDataElementDiscoveryForIdentifierAndFieldIdentfier(
    xtocpg: Try[Cpg],
    lang: Language
  ): List[List[String]] = {
    var workbookResult = new ListBuffer[List[String]]()
    try {
      // Adding Identifiers
      workbookResult = DataElementDiscoveryUtils.getIdentifiers(xtocpg, workbookResult, lang)

      // Adding FieldIdentifiers
      if (lang != Language.GO) {
        workbookResult = DataElementDiscoveryUtils.getFieldAccessIdentifier(xtocpg, workbookResult, lang)
      }
    } catch {
      case ex: Exception =>
        logger.debug("Failed to process Data Element Discovery report", ex)
    }

    DataElementDiscoveryUtils.filterEntriesWithEmptyMemberName(workbookResult.toList)
  }

  def processDataElementDiscovery(xtocpg: Try[Cpg], taggerCache: TaggerCache, lang: Language): List[List[String]] = {
    val classNameRuleList   = DataElementDiscoveryUtils.getSourceUsingRules(xtocpg)
    val memberInfo          = DataElementDiscoveryUtils.getMemberUsingClassName(xtocpg, classNameRuleList.toSet, lang)
    var workbookResult      = new ListBuffer[List[String]]()
    val typeDeclMemberCache = taggerCache.typeDeclMemberCache
    val methodParametersFromTypes = DataElementDiscoveryUtils.getMethodParametersFromTypes(
      xtocpg,
      classNameRuleList.toSet,
      AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX
    )
    val elementInfo = mutable.HashMap[TypeDecl, ListBuffer[Any]]()

    methodParametersFromTypes.foreach {
      case (typeDecl, paramList) => {
        // Filtering out the noise
        if (
          paramList.nonEmpty && !typeDecl.fullName.matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX)
        ) {
          if (!elementInfo.contains(typeDecl)) {
            elementInfo.put(typeDecl, new ListBuffer[Any])
          }
          val elements = elementInfo(typeDecl)
          paramList.foreach(param =>
            if (!param.name.matches(AuditReportConstants.JS_ELEMENTS_TO_BE_EXCLUDED)) {
              elements += param
            }
          )
          elementInfo.put(typeDecl, elements)
        }
      }
    }
    memberInfo.foreach {
      case (typeDecl, memberList) => {
        // Filtering out the noise
        if (
          memberList.nonEmpty && !typeDecl.fullName.matches(
            AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX
          )
        ) {
          if (!elementInfo.contains(typeDecl)) {
            elementInfo.put(typeDecl, new ListBuffer[Any])
          }
          val elements = elementInfo(typeDecl)
          memberList.foreach(member =>
            if (!member.name.matches(AuditReportConstants.JS_ELEMENTS_TO_BE_EXCLUDED)) {
              elements += member
            }
          )
          elementInfo.put(typeDecl, elements)
        }
      }
    }

    // Stores ClassName --> (MemberName --> SourceRuleID)
    val taggedMemberInfo = mutable.HashMap[String, mutable.HashMap[String, String]]()

    // Reverse the mapping to MemberName --> sourceRuleId
    typeDeclMemberCache.foreach { case (key, value) =>
      val reverseMap = mutable.HashMap[String, String]()
      value.foreach { case (ruleName, memberSet) =>
        memberSet.foreach(member => {
          reverseMap.put(member.name, ruleName)
        })
      }
      taggedMemberInfo.put(key, reverseMap)
    }

    // Construct the excel sheet and fill the data
    try {
      elementInfo.foreach {
        case (key, value) => {
          val (_, offset, nodeUniqueId) =
            DataElementDiscoveryUtils.getNodeLocationAndUniqueId(key, key.name, "")

          workbookResult += List(
            key.name,
            key.file.head.name,
            "0.0",
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            offset,
            nodeUniqueId,
            AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_TYPEDECL
          )
          val ruleMemberInfo = taggedMemberInfo.getOrElse(key.fullName, new mutable.HashMap[String, String])
          val addedMembers   = mutable.Set[String]()
          val addedParams    = mutable.Set[String]()

          value.foreach {
            case (member: Member) => {
              val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_MEMBER
              val (_, lineNumber, nodeUniqueId) =
                DataElementDiscoveryUtils.getNodeLocationAndUniqueId(member, member.name, nodeType)

              val memberUniqueKey =
                s"${key.fullName}${key.file.name.headOption.getOrElse(Constants.EMPTY)}${member.name}"

              if (member.name.nonEmpty && !addedMembers.contains(memberUniqueKey)) {
                addedMembers.add(memberUniqueKey)
                if (ruleMemberInfo.contains(member.name)) {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    "0.0",
                    member.name,
                    member.typeFullName,
                    AuditReportConstants.AUDIT_CHECKED_VALUE,
                    ruleMemberInfo.getOrElse(member.name, ""),
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    lineNumber,
                    nodeUniqueId,
                    nodeType
                  )
                } else {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    "0.0",
                    member.name,
                    member.typeFullName,
                    AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    lineNumber,
                    nodeUniqueId,
                    nodeType
                  )
                }
              }
            }
            case (param: MethodParameterIn) => {
              val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_METHOD_PARAM
              val (_, lineNumber, nodeUniqueId) =
                DataElementDiscoveryUtils.getNodeLocationAndUniqueId(param, param.name, nodeType)
              val paramUniqueKey = s"${key.fullName}${key.file.name.headOption.getOrElse(Constants.EMPTY)}${param.name}"

              if (!addedParams.contains(paramUniqueKey) && param.name.length > 2) {
                addedParams.add(paramUniqueKey)
                if (ruleMemberInfo.contains(param.name)) {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    "0.0",
                    param.name,
                    param.typeFullName,
                    AuditReportConstants.AUDIT_CHECKED_VALUE,
                    ruleMemberInfo.getOrElse(param.name, ""),
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    lineNumber,
                    nodeUniqueId,
                    nodeType
                  )
                } else {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    "0.0", // getFileScoreJS(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                    param.name,
                    param.typeFullName,
                    AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    lineNumber,
                    nodeUniqueId,
                    nodeType
                  )
                }
              }
            }
            case _ => {}
          }
        }
      }

      // Adding Identifiers
      workbookResult = DataElementDiscoveryUtils.getIdentifiers(xtocpg, workbookResult, lang)

      // Adding FieldIdentifiers
      if (lang != Language.GO) {
        workbookResult = DataElementDiscoveryUtils.getFieldAccessIdentifier(xtocpg, workbookResult, lang)
      }

      logger.info("Shutting down audit engine")
    } catch {
      case ex: Exception =>
        logger.debug("Failed to process Data Element Discovery report", ex)
    }
    DataElementDiscoveryUtils.getHeaderList() +: DataElementDiscoveryUtils.updateWorkbookResultsToGetUniqueSourcePerFile(workbookResult.toList)
  }
}
