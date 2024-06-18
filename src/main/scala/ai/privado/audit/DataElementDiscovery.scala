package ai.privado.audit

import ai.privado.cache.TaggerCache
import ai.privado.dataflow.Dataflow
import ai.privado.model.{Constants, InternalTag}
import ai.privado.model.Language
import ai.privado.model.Language.Language
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.util.Calendar
import scala.collection.Map
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DataElementDiscoveryUtils {

  private val logger = LoggerFactory.getLogger(getClass)
  // Not used for security-purposes. Only to generate a unique identifier.
  private lazy val md5 = java.security.MessageDigest.getInstance("MD5")

  // Regular expression pattern to filter common language types
  private val filterCommonLangTypes =
    "(?i)(class|window|str|list|dict|bool|chr|icon|boolean|number|bigdecimal|jwt|let|define|enum|asttype|nil|null|none|java|undefined|nan|empty|objectid|_id|tostring|valueof|json|mime|true|false|before|after|arr|typeof|match|to_dict|toarray|todate|case|<global>|<fakeNew>|(buf|array|int|num|type|float|byte|string|blob).{0,1}|set|map|hashmap|vector|deque|function|method|property|char|short|long|double|decimal|datetime|date|time|timestamp|enum|flag|void|interface|trait|package|namespace|record|tuple|struct|component|hook|hoc|service)"
  // Regular expression pattern to filter common variable names
  private val filterCommonVars =
    "(?i)(cls|self|ctx|constructor|prototype|main|use|foreach|copy|skip|replace|mongoose|slice|fill|some|every|concat|contains|apply|merge|stmt|format|name|data|regexp|.{0,1}(sort|in|slug|match|ne|regex|or|sum|and)|session|status|event|env|cmd|push|join|split|splice|filter|reduce|shift|unshift|retry|start|buffer|thread|length|staticmethod|app|next|end|req|console|push|pop|handler|server|catch|then|uri|split|exp|other|info|debug|warning|critical|exception|size|max|text|http|query|href|write|(sql|row|len|err|res|ret|obj|msg|val|key|item|url|tmp|col|file|img|test|result|path|module|import|export|log|key|value|include|load|dump).{0,1})"

  // List of prefixes to filter out common variables that start with these values
  private val filterCommonVarsStartsWith =
    "$obj|$group|$set|$id|__|_tmp_|tmp|$iterLocal|_result|file|is|sha_|this|get|set|post|put|update|create|clear|check|find|insert|assert|parse|generate|validate|process|download|upload|delete|handle|param|attr|arg|_iterator|{|log|error|iterator|logger|<tmp-|iter_tmp|toLocale|indexOf"
  private val filterCommonVarsStartsWithArr = filterCommonVarsStartsWith.split("\\|")

  // Additional Language specific filters
  def getLanguageSpecificFilters(lang: Language): String = {
    lang match {
      case Language.JAVASCRIPT =>
        "(?i)(<anon-class>.*|i18n|ajv|axios|require|express|moment|document|validationerror|hasOwnProperty|date|usestate|useeffect|dispatch|ngif|ngforof|inject|component|router|validators|formgroup|formcontrol|subscribe|observable|jquery|vue|react|angular)"
      case Language.PYTHON =>
        "(?i)(<module>|.*<meta>|print|boto.{0,1}|s3|requests|append|extend|list_objects.{0,6}|pytest|datetime|pandas|numpy|scipy|sklearn|matplotlib|flask|django|argparse|os|sys)"
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

  def getSourceRuleId(
    node: TypeDecl | Member | MethodParameterIn | Identifier | Local | FieldIdentifier | Call
  ): String = {
    // Extract the source rule ID, defaulting to AUDIT_EMPTY_CELL_VALUE if not found
    val sourceRuleId =
      node.tag.nameExact(Constants.id).value.headOption.getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE)

    // Determine if tagging is disabled by DED
    val taggingDisabledByDED = node.tag.nameExact(InternalTag.TAGGING_DISABLED_BY_DED.toString).nonEmpty

    if (taggingDisabledByDED) {
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

  // Add Members in workbookResult
  def getNewMembers(
    xtocpg: Try[Cpg],
    workbookResult: ListBuffer[List[String]],
    lang: Language
  ): ListBuffer[List[String]] = {
    val typeDeclMembersMap = xtocpg match {
      case Success(cpg) => {
        val memberInfoMap = mutable.HashMap[TypeDecl, List[Member]]()
        val members       = mutable.ListBuffer[Member]()
        cpg.typeDecl.foreach(typeDeclNode => {
          if (typeDeclNode.member.nonEmpty) {
            val filteredMembers =
              typeDeclNode.member
                // Filter out members with length <= 2
                .filter(i => i.name.length > 2)
                // Filter out members that start with any of the specified prefixes
                .filter(i => !filterCommonVarsStartsWithArr.exists(xx => i.name.toLowerCase.startsWith(xx.toLowerCase)))
                // Filter out members matching common language types pattern
                .filter(i => !i.name.matches(filterCommonLangTypes))
                // Filter out members matching common variable names pattern
                .filter(i => !i.name.matches(filterCommonVars))
                // Filter out members matching language-specific filters
                .filter(i => !i.name.matches(DataElementDiscoveryUtils.getLanguageSpecificFilters(lang)))
                .dedup
                .l
            memberInfoMap.put(typeDeclNode, filteredMembers)
            members ++= filteredMembers
          }
        })
        memberInfoMap.toMap
      }
      case Failure(ex) => {
        logger.debug(f"Error while getting new member ", ex)
        Map[TypeDecl, List[Member]]()
      }
    }

    val addedIdentifiers = mutable.Set[String]()
    typeDeclMembersMap.foreach { case (typeDecl, members) =>
      val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_CLASS_NAME
      val (_, offset, typeDeclNodeUniqueId) =
        DataElementDiscoveryUtils.getNodeLocationAndUniqueId(typeDecl, typeDecl.fullName, nodeType)

      if (members.nonEmpty) {
        workbookResult += List(
          typeDecl.name,
          typeDecl.file.head.name,
          "0.0",
          AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
          AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
          AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
          AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
          AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
          AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
          AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
          offset,
          typeDeclNodeUniqueId,
          AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_TYPEDECL
        )
      }

      members.foreach(member => {
        val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_MEMBER
        val (path, lineNumber, nodeUniqueId) =
          DataElementDiscoveryUtils.getNodeLocationAndUniqueId(member, member.name, nodeType)

        val identifierUniqueKey = s"${member.typeFullName}$path${member.name}"
        val sourceRuleId        = DataElementDiscoveryUtils.getSourceRuleId(member)

        if (
          !member.name
            .matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX)
          && !member.name
            .matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX)
          && !member.name.matches(AuditReportConstants.JS_ELEMENTS_TO_BE_EXCLUDED)
          && !addedIdentifiers.contains(identifierUniqueKey)
        )
          addedIdentifiers.add(identifierUniqueKey)
        workbookResult += List(
          typeDecl.fullName,
          typeDecl.file.name.headOption.getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE),
          "0.0",
          member.name,
          member.typeFullName,
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
    }

    workbookResult
  }

  // Add MethodParameters in workbookResult
  def getNewMethodParametersFromTypes(
    xtocpg: Try[Cpg],
    workbookResult: ListBuffer[List[String]],
    lang: Language
  ): ListBuffer[List[String]] = {
    val methodParameters = xtocpg match {
      case Success(cpg) => {
        val parameters = mutable.ListBuffer[MethodParameterIn]()
        cpg.method.foreach(typeDeclNode => {
          if (typeDeclNode.parameter.nonEmpty) {
            val filteredMembers =
              typeDeclNode.parameter
                // Filter out members with length <= 2
                .filter(i => i.name.length > 2)
                // Filter out members that start with any of the specified prefixes
                .filter(i => !filterCommonVarsStartsWithArr.exists(xx => i.name.toLowerCase.startsWith(xx.toLowerCase)))
                // Filter out members matching common language types pattern
                .filter(i => !i.name.matches(filterCommonLangTypes))
                // Filter out members matching common variable names pattern
                .filter(i => !i.name.matches(filterCommonVars))
                // Filter out members matching language-specific filters
                .filter(i => !i.name.matches(DataElementDiscoveryUtils.getLanguageSpecificFilters(lang)))
                .dedup
                .l
            parameters ++= filteredMembers
          }
        })
        parameters.toList
      }
      case Failure(ex) => {
        logger.debug(f"Error while getting new method param ", ex)
        List[MethodParameterIn]()
      }
    }

    val addedMethodParamaters = mutable.Set[String]()
    methodParameters.foreach(mParams => {
      val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_METHOD_PARAM
      val (path, lineNumber, nodeUniqueId) =
        DataElementDiscoveryUtils.getNodeLocationAndUniqueId(mParams, mParams.name, nodeType)

      val identifierUniqueKey = s"${mParams.typeFullName}$path${mParams.name}"
      val sourceRuleId        = DataElementDiscoveryUtils.getSourceRuleId(mParams)

      if (
        !mParams.name.matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX)
        && !mParams.name.matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX)
        && !mParams.name.matches(AuditReportConstants.JS_ELEMENTS_TO_BE_EXCLUDED)
        && !addedMethodParamaters.contains(identifierUniqueKey)
      )
        addedMethodParamaters.add(identifierUniqueKey)
      workbookResult += List(
        mParams.typeFullName,
        mParams.file.name.headOption.getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE),
        "0.0",
        mParams.name,
        mParams.typeFullName,
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

  // Add Identifiers in workbookResult
  def getIdentifiers(
    xtocpg: Try[Cpg],
    workbookResult: ListBuffer[List[String]],
    lang: Language
  ): ListBuffer[List[String]] = {
    val identifiers = xtocpg match {
      case Success(cpg) => {
        cpg.identifier
          .groupBy(i => (i.name, i.file.name.head))
          .map { case (_, identifiers) => identifiers.head }
          // Filter out identifiers with length <= 2
          .filter(i => i.name.length > 2)
          // Filter out identifiers that start with any of the specified prefixes
          .filter(i => !filterCommonVarsStartsWithArr.exists(xx => i.name.toLowerCase.startsWith(xx.toLowerCase)))
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
      val sourceRuleId        = DataElementDiscoveryUtils.getSourceRuleId(identifier)

      if (
        identifier.name.matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX)
        && !identifier.name.matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX)
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
    val identifierSourceRuleIdMap = new mutable.HashMap[Long, String]()
    val fieldIdentifiers = xtocpg match {
      case Success(cpg) => {
        cpg.fieldAccess.fieldIdentifier
          .groupBy(i => (i.canonicalName, i.file.name.head))
          .map { case (_, identifiers) => identifiers.head }
          // Filter out identifiers with length <= 2
          .filter(i => i.canonicalName.length > 2)
          // Filter out identifiers that start with any of the specified prefixes
          .filter(i =>
            !filterCommonVarsStartsWithArr.exists(xx => i.canonicalName.toLowerCase.startsWith(xx.toLowerCase))
          )
          // Filter out identifiers matching common language types pattern
          .filter(i => !i.canonicalName.matches(filterCommonLangTypes))
          // Filter out identifiers matching common variable names pattern
          .filter(i => !i.canonicalName.matches(filterCommonVars))
          // Filter out identifiers matching language-specific filters
          .filter(i => !i.canonicalName.matches(DataElementDiscoveryUtils.getLanguageSpecificFilters(lang)))
          // Filter out fieldIdentifiers like `session` of format `res.session.XXX`
          .whereNot(_.astParent.astParent.isCall.name("<operator>.fieldAccess"))
          .map((i: FieldIdentifier) => {
            if (i.astParent.isCall) {
              identifierSourceRuleIdMap
                .addOne(i.id, DataElementDiscoveryUtils.getSourceRuleId(i.astParent.asInstanceOf[Call]))
            }
            i
          })
          .l
      }
      case Failure(ex) => {
        logger.debug(f"Error while getting FieldIdentifier ", ex)
        List[FieldIdentifier]()
      }
    }

    val addedFieldIdentifiers = mutable.Set[String]()
    fieldIdentifiers.foreach(fIdentifier => {
      val nodeType = AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_FIELD_IDENTIFIER
      val (path, lineNumber, nodeUniqueId) =
        DataElementDiscoveryUtils.getNodeLocationAndUniqueId(fIdentifier, fIdentifier.canonicalName, nodeType)
      val localsUniqueKey = s"NA$path${fIdentifier.canonicalName}"
      val sourceRuleId =
        identifierSourceRuleIdMap.get(fIdentifier.id).getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE)

      if (
        !fIdentifier.canonicalName.matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX)
        && !fIdentifier.canonicalName.matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX)
        && !fIdentifier.canonicalName.matches(AuditReportConstants.JS_ELEMENTS_TO_BE_EXCLUDED)
        && !addedFieldIdentifiers.contains(localsUniqueKey)
      )
        addedFieldIdentifiers.add(localsUniqueKey)
      workbookResult += List(
        "NA",
        fIdentifier.file.head.name,
        "0.0",
        fIdentifier.canonicalName,
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
                      .filter(i =>
                        !filterCommonVarsStartsWithArr.exists(xx => i.name.toLowerCase.startsWith(xx.toLowerCase))
                      )
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
      // Group entries by filename
      val groupedByFilename = workbookResult.groupBy(entry => entry(1))

      // For each group, filter unique items based on name
      val filteredResults = groupedByFilename.values.flatMap { entries =>
        val uniqueByName = entries.groupBy(entry => entry(3)).values.map(_.head)
        uniqueByName
      }.toList
      filteredResults
    } catch {
      case e: Exception =>
        logger.debug(s"An error occurred while filtering the workbook result: ${e.getMessage}")
        workbookResult
    }
  }
}

object DataElementDiscovery {
  private val logger = LoggerFactory.getLogger(getClass)

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

    DataElementDiscoveryUtils.updateWorkbookResultsToGetUniqueSourcePerFile(workbookResult.toList)
  }

  def processDataElementDiscovery(xtocpg: Try[Cpg], taggerCache: TaggerCache, lang: Language): List[List[String]] = {
    var workbookResult       = new ListBuffer[List[String]]()
    var memberWorkbookResult = new ListBuffer[List[String]]()

    // Construct the excel sheet and fill the data
    try {
      // Adding Members
      println(s"${Calendar.getInstance().getTime} - Member Sources Query Started...")
      memberWorkbookResult = DataElementDiscoveryUtils.getNewMembers(xtocpg, memberWorkbookResult, lang)
      println(s"${Calendar.getInstance().getTime} - new Member size: ${memberWorkbookResult.size}")
      println(s"${Calendar.getInstance().getTime} - New Member Sources COMPLETED...")

      if (lang != Language.JAVA && lang != Language.KOTLIN) {
        // Adding MethodParameters
        workbookResult = DataElementDiscoveryUtils.getNewMethodParametersFromTypes(xtocpg, workbookResult, lang)
        println(s"${Calendar.getInstance().getTime} - new Method Param size: ${workbookResult.size}")
        println(s"${Calendar.getInstance().getTime} - New Method Param Sources COMPLETED...")
      }

      // Adding Identifiers
      workbookResult = DataElementDiscoveryUtils.getIdentifiers(xtocpg, workbookResult, lang)
      println(s"${Calendar.getInstance().getTime} - Identifier Sources COMPLETED...")
      println(s"${Calendar.getInstance().getTime} - new Identifier size: ${workbookResult.size}")

      // Adding FieldIdentifiers
      if (lang != Language.GO && lang != Language.JAVA && lang != Language.KOTLIN) {
        workbookResult = DataElementDiscoveryUtils.getFieldAccessIdentifier(xtocpg, workbookResult, lang)
        println(s"${Calendar.getInstance().getTime} - Field Identifier Sources COMPLETED...")
        println(s"${Calendar.getInstance().getTime} - new Field Identifier size: ${workbookResult.size}")
      }

      logger.info("Shutting down audit engine")
    } catch {
      case ex: Exception =>
        println(s"Failed to process Data Element Discovery report ${ex}")
    }
    println(s"No of sources: ${workbookResult.size + memberWorkbookResult.size}")
    memberWorkbookResult = DataElementDiscoveryUtils.getHeaderList() +: memberWorkbookResult
    memberWorkbookResult.toList ++ DataElementDiscoveryUtils
      .updateWorkbookResultsToGetUniqueSourcePerFile(workbookResult.toList)
  }
}
