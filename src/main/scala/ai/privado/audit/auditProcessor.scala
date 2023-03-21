package ai.privado.audit

import ai.privado.cache.TaggerCache
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Member
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object auditProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  // Get list of Class Name
  private def getClassName(xtocpg: Try[Cpg]): List[String] = {
    logger.info("Process Class Name from cpg")
    val sourceList = ListBuffer[String]()
    xtocpg match {
      case Success(cpg) => {
        val typeDeclList = cpg.typeDecl.filter(_.order > 0).toList
        typeDeclList.foreach(node => {
          if (node.head.fullName.nonEmpty) {
            sourceList += node.fullName
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
    sourceList.toList
  }

  // Get list of member variable present in given class
  private def getMemberUsingClassName(
    xtocpg: Try[Cpg],
    classInfoList: List[String]
  ): mutable.Map[String, List[Member]] = {
    logger.info("Process Member Name from cpg")
    val memberInfoMap = mutable.Map[String, List[Member]]()

    xtocpg match {
      case Success(cpg) => {
        classInfoList.foreach(className => {
          val memberInfo = cpg.member.filter(_.typeDecl.order > 0).where(_.typeDecl.fullName(className)).l
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

  def processTagMember(xtocpg: Try[Cpg]): List[List[String]] = {
    logger.info("Initiated the audit engine")
    val classNameList       = getClassName(xtocpg)
    val memberInfo          = getMemberUsingClassName(xtocpg, classNameList)
    val result              = new ListBuffer[List[String]]()
    val typeDeclMemberCache = TaggerCache.typeDeclMemberCache

    // Stores ClassName --> (MemberName --> SourceRuleID)
    val taggedMemberInfo = mutable.HashMap[String, mutable.HashMap[String, String]]()

    // Reverse the mapping to MemberName --> sourceRuleId
    typeDeclMemberCache.foreach { case (key, value) =>
      val reverseMap = mutable.HashMap[String, String]()
      value.foreach { case (ruleName, member) =>
        reverseMap.put(member.name, ruleName)
      }
      taggedMemberInfo.put(key, reverseMap)
    }

    // Header List
    result += List("Class", "Member", "Tagged", "Source Rule ID")

    // Construct the excel sheet and fill the data
    try {
      memberInfo.foreach {
        case (key, value) => {
          if (taggedMemberInfo.contains(key)) {
            result += List(key, "--", "YES", "")
            val ruleMemberInfo = taggedMemberInfo.getOrElse(key, new mutable.HashMap[String, String])
            value.foreach(member => {
              if (ruleMemberInfo.contains(member.name)) {
                result += List("", member.name, "YES", ruleMemberInfo.getOrElse(member.name, "Default value"))
              } else {
                result += List("", member.name, "NO", "--")
              }
            })
          } else {
            result += List(key, "--", "NO", "")
            value.foreach(member => {
              result += List("", member.name, "NO", "--")
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
    result.toList
  }
}
