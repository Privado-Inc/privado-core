package ai.privado.audit

import ai.privado.cache.TaggerCache
import ai.privado.model.InternalTag
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Member, MethodParameterIn, TypeDecl}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DataElementDiscovery {

  private val logger = LoggerFactory.getLogger(getClass)

  private val excludeClassNameRegex = "^(.*)(Controller|Service|Impl|Helper|Util|Processor)$"

  // Get list of Class Name having getter and setter method
  def getSourceUsingRules(xtocpg: Try[Cpg]): List[String] = {
    logger.info("Process Class Name from cpg")
    val classNameList = ListBuffer[String]()
    xtocpg match {
      case Success(cpg) => {
        // Get DTO/Entity Class name
        val typeDeclList = cpg.typeDecl
          .filter(_.order > 0)
          .whereNot(_.name(excludeClassNameRegex))
          .or(
            _.where(_.method.name("^(get|set).*")),
            _.where(_.method.name("^(hascode|equals)")),
            _.where(_.annotation.name(".*(Getter|Setter).*"))
          )
          .toList
        typeDeclList.foreach(node => {
          if (node.head.fullName.nonEmpty) {
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
            .whereNot(_.name(excludeClassNameRegex))
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
  def getMemberUsingClassName(xtocpg: Try[Cpg], classNameSet: Set[String]): mutable.Map[TypeDecl, List[Member]] = {
    logger.info("Process Member Name from cpg")
    val memberInfoMap = mutable.Map[TypeDecl, List[Member]]()

    xtocpg match {
      case Success(cpg) => {
        classNameSet.foreach(className => {
          // Get member variable of class and put it into map
          val classInfo = cpg.typeDecl.filter(_.order > 0).where(_.fullName(className)).l
          if (classInfo.nonEmpty) {
            val memberInfo = classInfo.head.member.toList
            memberInfoMap.put(classInfo.head, memberInfo)
          }
        })
      }
      case Failure(exception) => {
        println("Failed to process member name from cpg")
        logger.debug("Failed to process member name from cpg", exception)
        println(exception.printStackTrace())
      }
    }
    logger.info("Successfully Processed member name from cpg")
    memberInfoMap
  }

  // Get Collection Input Class Name
  def getCollectionInputList(xtocpg: Try[Cpg]): List[String] = {
    val collectionInputList = ListBuffer[String]()

    xtocpg match {
      case Success(cpg) => {
        // Get tagged collection input list
        val parameterList = cpg.parameter.where(_.tag).l
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

  def getCollectionMethodInfo(xtocpg: Try[Cpg]): Map[String, CollectionMethodInfo] = {
    // className -> (MethodName, Endpoint)
    val collectionMethodInfoMap = new mutable.HashMap[String, CollectionMethodInfo]()
    xtocpg match {
      case Success(cpg) => {
        val parameterList = cpg.parameter.where(_.tag).l
        parameterList.foreach(parameter => {

          // Get complete Endpoint path
          val endpointTag = parameter.method.tag.where(_.name(InternalTag.COLLECTION_METHOD_ENDPOINT.toString)).head
          // Append endpoint and method name when Method parameter having same type
          if (!collectionMethodInfoMap.contains(parameter.typeFullName)) {
            collectionMethodInfoMap
              .put(parameter.typeFullName, CollectionMethodInfo(parameter.method.name, endpointTag.value))
          } else {
            val collectionMethodInfo = collectionMethodInfoMap(parameter.typeFullName)
            collectionMethodInfo.endpoint = s"${collectionMethodInfo.endpoint}\n${endpointTag.value}"
            collectionMethodInfo.methodName = s"${collectionMethodInfo.methodName}\n${parameter.method.name}"
          }
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
    workbookResult += List(
      "Class",
      "File Name",
      "Member",
      "Member Type",
      "Tagged",
      "Source Rule ID",
      "Input to Collection",
      "Collection Endpoint path",
      "Collection Method Full Name"
    )

    // Construct the excel sheet and fill the data
    try {
      memberInfo.foreach {
        case (key, value) => {
          val isCollectionInput = if (collectionInputList.contains(key.fullName)) "YES" else "NO"
          val collectionMethodName =
            if (collectionMethodInfo.contains(key.fullName)) collectionMethodInfo(key.fullName).methodName else "--"
          val collectionEndPointPath =
            if (collectionMethodInfo.contains(key.fullName)) collectionMethodInfo(key.fullName).endpoint else "--"
          if (taggedMemberInfo.contains(key.fullName)) {
            workbookResult += List(
              key.fullName,
              key.file.head.name,
              "--",
              "--",
              "YES",
              "--",
              isCollectionInput,
              collectionEndPointPath,
              collectionMethodName
            )
            val ruleMemberInfo = taggedMemberInfo.getOrElse(key.fullName, new mutable.HashMap[String, String])
            value.foreach(member => {
              if (ruleMemberInfo.contains(member.name)) {
                workbookResult += List(
                  "",
                  "",
                  member.name,
                  member.typeFullName,
                  "YES",
                  ruleMemberInfo.getOrElse(member.name, "Default value"),
                  "",
                  "",
                  ""
                )
              } else {
                workbookResult += List("", "", member.name, member.typeFullName, "NO", "--", "", "", "")
              }
            })
          } else {
            workbookResult += List(
              key.fullName,
              key.file.head.name,
              "--",
              "--",
              "NO",
              "--",
              isCollectionInput,
              collectionEndPointPath,
              collectionMethodName
            )
            value.foreach(member => {
              workbookResult += List("", "", member.name, member.typeFullName, "NO", "--", "", "", "")
            })
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

  case class CollectionMethodInfo(var methodName: String, var endpoint: String)
}
