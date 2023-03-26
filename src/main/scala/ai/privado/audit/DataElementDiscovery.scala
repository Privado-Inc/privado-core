package ai.privado.audit

import ai.privado.cache.TaggerCache
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Member, MethodParameterIn}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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
          .whereNot(_.name("^(.*)(Controller|Service|Impl|Helper|Util|Processor)$"))
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
            .whereNot(_.name("^(.*)(Controller|Service|Impl|Helper|Util|Processor)$"))
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
  def getMemberUsingClassName(xtocpg: Try[Cpg], classNameSet: Set[String]): mutable.Map[String, List[Member]] = {
    logger.info("Process Member Name from cpg")
    val memberInfoMap = mutable.Map[String, List[Member]]()

    xtocpg match {
      case Success(cpg) => {
        classNameSet.foreach(className => {
          // Get member variable of class and put it into map
          // val memberInfo = cpg.member.filter(_.typeDecl.order > 0).where(_.typeDecl.fullName(className)).l
          val memberInfo = cpg.typeDecl.filter(_.order > 0).where(_.fullName(className)).member.toList
          memberInfoMap.put(className, memberInfo)
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
        // val parameterIn = cpg.method.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.COLLECTIONS.name)).parameter.l
        // Get tagged collection input list
        val methodList = cpg.parameter.where(_.tag).l
        methodList.foreach(method => {
          collectionInputList += method.typeFullName
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

  def processDataElementDiscovery(xtocpg: Try[Cpg], taggerCache: TaggerCache): List[List[String]] = {
    logger.info("Initiated the audit engine")
    val classNameRuleList   = getSourceUsingRules(xtocpg)
    val collectionInputList = getCollectionInputList(xtocpg)
    val derivedClassName    = extractClassFromPackage(xtocpg, (classNameRuleList ++ collectionInputList).toSet)
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
    workbookResult += List("Class", "Member", "Tagged", "Source Rule ID", "Input to Collection")

    // Construct the excel sheet and fill the data
    try {
      memberInfo.foreach {
        case (key, value) => {
          val isCollectionInput = if (collectionInputList.contains(key)) "YES" else "NO"
          if (taggedMemberInfo.contains(key)) {
            workbookResult += List(key, "--", "YES", "", isCollectionInput)
            val ruleMemberInfo = taggedMemberInfo.getOrElse(key, new mutable.HashMap[String, String])
            value.foreach(member => {
              if (ruleMemberInfo.contains(member.name)) {
                workbookResult += List(
                  "",
                  member.name,
                  "YES",
                  ruleMemberInfo.getOrElse(member.name, "Default value"),
                  ""
                )
              } else {
                workbookResult += List("", member.name, "NO", "--", "")
              }
            })
          } else {
            workbookResult += List(key, "--", "NO", "", isCollectionInput)
            value.foreach(member => {
              workbookResult += List("", member.name, "NO", "--", "")
            })
          }
        }
      }
      logger.info("Shutting down audit engine")
    } catch {
      case ex: Exception =>
        println("Failed to process audit report")
        logger.debug("Failed to process audit report", ex)
    }
    workbookResult.toList
  }
}
