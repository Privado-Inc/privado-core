package ai.privado.audit

import ai.privado.audit.DataElementDiscovery.{CollectionMethodInfo, getClass, getFileScore, getSourceUsingRules, logger}
import ai.privado.cache.TaggerCache
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{File, Identifier, Local, Member, MethodParameterIn, TypeDecl}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import ai.privado.dataflow.Dataflow

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object DataElementDiscovery {

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
        println("Failed to process class name from cpg")
        logger.debug("Failed to process class name from cpg", exception)
        println(exception.printStackTrace())
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
        println("Failed to Extract package classes from cpg")
        logger.debug("Failed to Extract package classes from cpg", exception)
        println(exception.printStackTrace())
      }
    }
    derivedClassName.toSet
  }

  // Get list of member variable present in given class
  def getMemberUsingClassName(xtocpg: Try[Cpg], classNameSet: Set[String]): Map[TypeDecl, List[Member]] = {
    logger.info("Process Member Name from cpg")
    val memberInfoMap = mutable.HashMap[TypeDecl, List[Member]]()

    xtocpg match {
      case Success(cpg) => {
        classNameSet.foreach(className => {
          cpg.typeDecl
            .where(_.fullName(className))
            .foreach(typeDeclNode => memberInfoMap.put(typeDeclNode, typeDeclNode.member.l))
        })
      }
      case Failure(exception) => {
        println("Failed to process member name from cpg")
        logger.debug("Failed to process member name from cpg", exception)
        println(exception.printStackTrace())
      }
    }
    logger.info("Successfully Processed member name from cpg")
    memberInfoMap.toMap
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
        println("Failed to process method parameter from cpg")
        logger.debug("Failed to process method parameter from cpg", exception)
        println(exception.printStackTrace())
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
        println("Failed to process collection method info from cpg")
        logger.debug("Failed to process collection method info from cpg", exception)
        println(exception.printStackTrace())
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
        println("Failed to calculate file score")
        logger.debug("Failed to calculate file score", exception)
        logger.debug("exception: ", exception.printStackTrace())
        "0"
      }
    }
  }

  def processDataElementDiscovery(xtocpg: Try[Cpg], taggerCache: TaggerCache): List[List[String]] = {
    logger.info("Initiated the audit engine")
    val classNameRuleList    = getSourceUsingRules(xtocpg)
    val collectionInputList  = getCollectionInputList(xtocpg)
    val collectionMethodInfo = getCollectionMethodInfo(xtocpg)
    val derivedClassName     = extractClassFromPackage(xtocpg, (classNameRuleList ++ collectionInputList).toSet)
    val memberInfo =
      getMemberUsingClassName(xtocpg, (classNameRuleList ++ collectionInputList ++ derivedClassName).toSet)
    val workbookResult      = new ListBuffer[List[String]]()
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

    // Header List
    // rearranging this list will affect the ordering on audit-sources.json file
    workbookResult += List(
      AuditReportConstants.ELEMENT_DISCOVERY_CLASS_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_FILE_NAME,
      AuditReportConstants.FILE_PRIORITY_SCORE,
      AuditReportConstants.ELEMENT_DISCOVERY_MEMBER_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_MEMBER_TYPE,
      AuditReportConstants.ELEMENT_DISCOVERY_TAGGED_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_SOURCE_RULE_ID,
      AuditReportConstants.ELEMENT_DISCOVERY_INPUT_COLLECTION,
      AuditReportConstants.ELEMENT_DISCOVERY_COLLECTION_ENDPOINT,
      AuditReportConstants.ELEMENT_DISCOVERY_METHOD_NAME
    )

    // Construct the excel sheet and fill the data
    try {
      memberInfo.foreach {
        case (key, value) => {
          val isCollectionInput = if (collectionInputList.contains(key.fullName)) "YES" else "NO"
          if (taggedMemberInfo.contains(key.fullName)) {
            if (collectionMethodInfo.contains(key.fullName)) {
              collectionMethodInfo(key.fullName).foreach(info => {
                workbookResult += List(
                  key.fullName,
                  key.file.head.name,
                  getFileScore(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_CHECKED_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  isCollectionInput,
                  info.endpoint,
                  info.methodDetail
                )
              })
            } else {
              workbookResult += List(
                key.fullName,
                key.file.head.name,
                getFileScore(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_CHECKED_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                isCollectionInput,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
              )
            }
            val ruleMemberInfo = taggedMemberInfo.getOrElse(key.fullName, new mutable.HashMap[String, String])
            value.foreach {
              case (member: Member) => {
                if (ruleMemberInfo.contains(member.name)) {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    getFileScore(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                    member.name,
                    member.typeFullName,
                    AuditReportConstants.AUDIT_CHECKED_VALUE,
                    ruleMemberInfo.getOrElse(member.name, "Default value"),
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
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
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
                  )
                }
              }
            }
          } else {
            if (collectionMethodInfo.contains(key.fullName)) {
              collectionMethodInfo(key.fullName).foreach(info => {
                workbookResult += List(
                  key.fullName,
                  key.file.head.name,
                  getFileScore(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                  isCollectionInput,
                  info.endpoint,
                  info.methodDetail
                )
              })
            } else {
              workbookResult += List(
                key.fullName,
                key.file.head.name,
                getFileScore(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                isCollectionInput,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
              )
            }
            value.foreach {
              case (member: Member) => {
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
                  AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
                )
              }
            }
          }
        }
      }
      logger.info("Shutting down audit engine")
    } catch {
      case ex: Exception =>
        println("Failed to process Data Element Discovery report")
        logger.debug("Failed to process Data Element Discovery report", ex)
    }
    workbookResult.toList
  }

  case class CollectionMethodInfo(var methodDetail: String, var endpoint: String)

}

object DataElementDiscoveryJS {
  private val logger = LoggerFactory.getLogger(getClass)

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
        println("Failed to process class name from cpg")
        logger.debug("Failed to process class name from cpg", exception)
        println(exception.printStackTrace())
      }
    }
    logger.info("Successfully Processed Class Name from cpg")
    classNameList.toList
  }

  def getMethodParametersFromTypes(
    xtocpg: Try[Cpg],
    classNameRuleList: Set[String]
  ): Map[TypeDecl, ListBuffer[MethodParameterIn]] = {
    val methodParameterMap = new mutable.HashMap[TypeDecl, ListBuffer[MethodParameterIn]]()
    xtocpg match {
      case Success(cpg) => {
        classNameRuleList.foreach(className => {
          cpg.typeDecl.where(_.fullName(className)).l.headOption match
            case Some(typeDecl) =>
              cpg.method
                .fullName(typeDecl.fullName)
                .foreach(method => {
                  if (!methodParameterMap.contains(typeDecl)) {
                    methodParameterMap.put(typeDecl, new ListBuffer[MethodParameterIn])
                  }
                  val methodParameterInfo = methodParameterMap(typeDecl)
                  method.parameter.l
                    .whereNot(_.name(AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX))
                    .foreach(parameter => {
                      methodParameterInfo += parameter
                    })
                  methodParameterMap.put(typeDecl, methodParameterInfo)
                })
            case None =>
              logger.debug("head of empty list")
        })
      }
      case Failure(exception) => {
        println("Failed to process method parameter info from cpg")
        logger.debug("Failed to process method parameter info from cpg", exception)
        exception.printStackTrace()
      }
    }
    methodParameterMap.toMap
  }

  def getIdentifiers(xtocpg: Try[Cpg]): List[Identifier] = {
    xtocpg match {
      case Success(cpg) => {
        cpg.identifier.l
      }
      case Failure(ex) => {
        ex.printStackTrace()
        List[Identifier]()
      }
    }
  }

  def getLocals(xtocpg: Try[Cpg]): List[Local] = {
    xtocpg match {
      case Success(cpg) => {
        cpg.local.l
      }
      case Failure(ex) => {
        ex.printStackTrace()
        List[Local]()
      }
    }
  }

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

  def processDataElementDiscovery(xtocpg: Try[Cpg], taggerCache: TaggerCache): List[List[String]] = {
    val classNameRuleList         = getSourceUsingRules(xtocpg)
    val memberInfo                = DataElementDiscovery.getMemberUsingClassName(xtocpg, classNameRuleList.toSet)
    val workbookResult            = new ListBuffer[List[String]]()
    val typeDeclMemberCache       = taggerCache.typeDeclMemberCache
    val methodParametersFromTypes = getMethodParametersFromTypes(xtocpg, classNameRuleList.toSet)
    val identifiers               = getIdentifiers(xtocpg)
    val locals                    = getLocals(xtocpg)
    val elementInfo               = mutable.HashMap[TypeDecl, ListBuffer[Any]]()

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

    // Header List
    workbookResult += List(
      AuditReportConstants.ELEMENT_DISCOVERY_CLASS_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_FILE_NAME,
      AuditReportConstants.FILE_PRIORITY_SCORE,
      AuditReportConstants.ELEMENT_DISCOVERY_MEMBER_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_MEMBER_TYPE,
      AuditReportConstants.ELEMENT_DISCOVERY_TAGGED_NAME,
      AuditReportConstants.ELEMENT_DISCOVERY_SOURCE_RULE_ID,
      AuditReportConstants.ELEMENT_DISCOVERY_INPUT_COLLECTION,
      AuditReportConstants.ELEMENT_DISCOVERY_COLLECTION_ENDPOINT,
      AuditReportConstants.ELEMENT_DISCOVERY_METHOD_NAME
    )
    // Construct the excel sheet and fill the data
    try {
      elementInfo.foreach {
        case (key, value) => {
          workbookResult += List(
            key.name,
            key.file.head.name,
            getFileScoreJS(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_CHECKED_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
          )
          val ruleMemberInfo = taggedMemberInfo.getOrElse(key.fullName, new mutable.HashMap[String, String])
          val addedMembers   = mutable.Set[String]()
          val addedParams    = mutable.Set[String]()

          value.foreach {
            case (member: Member) => {
              val memberUniqueKey =
                s"${key.fullName}${key.file.name.headOption.getOrElse(Constants.EMPTY)}${member.name}"
              if (member.name.nonEmpty && !addedMembers.contains(memberUniqueKey)) {
                addedMembers.add(memberUniqueKey)
                if (ruleMemberInfo.contains(member.name)) {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    getFileScoreJS(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                    member.name,
                    member.typeFullName,
                    AuditReportConstants.AUDIT_CHECKED_VALUE,
                    ruleMemberInfo.getOrElse(member.name, "Default value"),
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
                  )
                } else {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    getFileScoreJS(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                    member.name,
                    member.typeFullName,
                    AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
                  )
                }
              }
            }

            case (param: MethodParameterIn) => {
              val paramUniqueKey = s"${key.fullName}${key.file.name.headOption.getOrElse(Constants.EMPTY)}${param.name}"
              if (!addedParams.contains(paramUniqueKey)) {
                addedParams.add(paramUniqueKey)
                if (ruleMemberInfo.contains(param.name)) {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    getFileScoreJS(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                    param.name,
                    param.typeFullName,
                    AuditReportConstants.AUDIT_CHECKED_VALUE,
                    ruleMemberInfo.getOrElse(param.name, "Default value"),
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
                  )
                } else {
                  workbookResult += List(
                    key.fullName,
                    key.file.head.name,
                    getFileScoreJS(key.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
                    param.name,
                    param.typeFullName,
                    AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
                    AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
                  )
                }
              }
            }
            case _ => {}
          }
        }
      }
      val addedIdentifiers = mutable.Set[String]()
      identifiers.foreach(identifier => {
        val identifierUniqueKey =
          s"${identifier.typeFullName}${identifier.file.name.headOption.getOrElse(Constants.EMPTY)}${identifier.name}"
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
            getFileScoreJS(identifier.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
            identifier.name,
            identifier.typeFullName,
            AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
          )
      })

      val addedLocals = mutable.Set[String]()
      locals.foreach(local => {
        val localsUniqueKey =
          s"${local.typeFullName}${local.file.name.headOption.getOrElse(Constants.EMPTY)}${local.name}"
        if (
          local.name.nonEmpty && !local.name
            .matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_TYPE_EXCLUDE_REGEX) && !local.name
            .matches(AuditReportConstants.JS_ELEMENT_DISCOVERY_EXCLUDE_PARAMS_REGEX)
          && !local.name.matches(AuditReportConstants.JS_ELEMENTS_TO_BE_EXCLUDED)
          && !addedLocals.contains(localsUniqueKey)
        )
          addedLocals.add(localsUniqueKey)
          workbookResult += List(
            local.typeFullName,
            local.file.head.name,
            getFileScoreJS(local.file.name.headOption.getOrElse(Constants.EMPTY), xtocpg),
            local.name,
            local.typeFullName,
            AuditReportConstants.AUDIT_NOT_CHECKED_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
            AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
          )
      })

      logger.info("Shutting down audit engine")
    } catch {
      case ex: Exception =>
        println("Failed to process Data Element Discovery report")
        logger.debug("Failed to process Data Element Discovery report", ex)
        ex.printStackTrace()
    }
    workbookResult.toList
  }
}
