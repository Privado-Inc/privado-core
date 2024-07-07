package ai.privado.audit

import ai.privado.cache.TaggerCache
import ai.privado.dataflow.Dataflow
import ai.privado.model.Language.Language
import ai.privado.model.{CatLevelOne, Constants, InternalTag, Language}
import ai.privado.semantic.Language.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DataElementDiscoveryUtils {

  private val logger = LoggerFactory.getLogger(getClass)

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
                        !DEDSourceDiscoveryUtils.filterCommonVarsStartsWithArr.exists(xx =>
                          i.name.toLowerCase.startsWith(xx.toLowerCase)
                        )
                      )
                      // Filter out members matching common language types pattern
                      .filter(i => !i.name.matches(DEDSourceDiscoveryUtils.filterCommonLangTypes))
                      // Filter out members matching common variable names pattern
                      .filter(i => !i.name.matches(DEDSourceDiscoveryUtils.filterCommonVars))
                      // Filter out members matching language-specific filters
                      .filter(i => !i.name.matches(DEDSourceDiscoveryUtils.getLanguageSpecificFilters(lang)))
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

  def appendSqlNodesToWorkbook(xtocpg: Try[Cpg], workbookResult: ListBuffer[List[String]]): Unit = xtocpg match {
    case Success(cpg) =>
      val sqlNodes = cpg.sqlColumn.l
      sqlNodes.foreach { sqlNode =>
        val tableName = sqlNode.code
        val shouldFilterNode = tableName.length <= 2
          || DEDSourceDiscoveryUtils.filterCommonVarsStartsWithArr.exists(commonVar => tableName.startsWith(commonVar))
          || tableName.matches(DEDSourceDiscoveryUtils.filterCommonLangTypes)
          || tableName.matches(DEDSourceDiscoveryUtils.filterCommonVars)

        if (!shouldFilterNode) {
          val path       = sqlNode.file.name.headOption.getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE)
          val lineNumber = sqlNode.lineNumber.getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE).toString
          val nodeUniqueId = DEDSourceDiscoveryUtils.nodeIdentifier(
            path,
            sqlNode.code,
            AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_SQL_NODE,
            lineNumber
          )

          val sourceRuleId = sqlNode.tag
            .nameExact(Constants.id)
            .value
            .headOption
            .getOrElse(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE)

          workbookResult += List(
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            path,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            sqlNode.code,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            sourceRuleId.startsWith("Data.Sensitive.").toString,
            sourceRuleId,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            lineNumber,
            nodeUniqueId,
            AuditReportConstants.ELEMENT_DISCOVERY_NODE_TYPE_SQL_NODE
          )
        }
      }

      logger.info("Appended SQL nodes to the workbook")

    case _ => logger.debug("Did not receive a valid cpg")
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
              DEDSourceDiscoveryUtils.getNodeLocationAndUniqueId(key, key.fullName, nodeType)

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
                  DEDSourceDiscoveryUtils.getNodeLocationAndUniqueId(member, member.name, nodeType)

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
              DEDSourceDiscoveryUtils.getNodeLocationAndUniqueId(key, key.fullName, nodeType)

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
                  DEDSourceDiscoveryUtils.getNodeLocationAndUniqueId(member, member.name, nodeType)

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
      workbookResult = DEDSourceDiscoveryUtils.getIdentifiers(xtocpg, workbookResult, lang)
      DataElementDiscoveryUtils.appendSqlNodesToWorkbook(xtocpg, workbookResult)

      logger.info("Shutting down audit engine")
    } catch {
      case ex: Exception =>
        logger.debug("Failed to process Data Element Discovery report", ex)
    }
    DEDSourceDiscoveryUtils.getHeaderList() +: DataElementDiscoveryUtils
      .updateWorkbookResultsToGetUniqueSourcePerFile(workbookResult.toList)
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
      workbookResult = DEDSourceDiscoveryUtils.getIdentifiers(xtocpg, workbookResult, lang)

      // Adding FieldIdentifiers
      if (lang != Language.GO) {
        workbookResult = DEDSourceDiscoveryUtils.getFieldAccessIdentifier(xtocpg, workbookResult, lang)
      }
    } catch {
      case ex: Exception =>
        logger.debug("Failed to process Data Element Discovery report", ex)
    }

    workbookResult.toList
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
            DEDSourceDiscoveryUtils.getNodeLocationAndUniqueId(key, key.name, "")

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
                DEDSourceDiscoveryUtils.getNodeLocationAndUniqueId(member, member.name, nodeType)

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
                DEDSourceDiscoveryUtils.getNodeLocationAndUniqueId(param, param.name, nodeType)
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
      workbookResult = DEDSourceDiscoveryUtils.getIdentifiers(xtocpg, workbookResult, lang)

      // Adding FieldIdentifiers
      if (lang != Language.GO) {
        workbookResult = DEDSourceDiscoveryUtils.getFieldAccessIdentifier(xtocpg, workbookResult, lang)
      }

      DataElementDiscoveryUtils.appendSqlNodesToWorkbook(xtocpg, workbookResult)
      logger.info("Shutting down audit engine")
    } catch {
      case ex: Exception =>
        logger.debug("Failed to process Data Element Discovery report", ex)
    }
    DEDSourceDiscoveryUtils.getHeaderList() +: DataElementDiscoveryUtils
      .updateWorkbookResultsToGetUniqueSourcePerFile(workbookResult.toList)
  }
}
