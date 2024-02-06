package ai.privado.audit

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import io.shiftleft.semanticcpg.language.*

import scala.util.{Success, Try}

object APIReport {

  private val logger = LoggerFactory.getLogger(getClass)

  def processAPIAudit(xtocpg: Try[Cpg], ruleCache: RuleCache): List[List[String]] = {
    val COMMON_IGNORED_SINKS_REGEX = ruleCache.getSystemConfigByKey(Constants.ignoredSinks)
    val APISINKS_REGEX             = ruleCache.getSystemConfigByKey(Constants.apiSinks)
    val COMMON_HTTP_PACKAGE_REGEX: String =
      ".*(?i)(client|http|api|instance|fetch|request|requests|service|rest|network|connector|communicator|adaptor|controller|handler|web|remote|caller|connection).*"

    val workFlowResult = new ListBuffer[List[String]]
    xtocpg match {
      case Success(cpg) => {
        val cacheCall = cpg.call.where(_.nameNot("(<operator|<init).*")).l
        val apis = cacheCall
          .name(APISINKS_REGEX)
          .methodFullNameNot(COMMON_IGNORED_SINKS_REGEX)
          .code(COMMON_HTTP_PACKAGE_REGEX)
          .whereNot(_.tag.where(_.nameExact(Constants.catLevelTwo).value(Constants.API)))
          .l

        apis.foreach(call => {
          workFlowResult += List(call.code, call.file.head.name, call.lineNumber.getOrElse(0).toString)
        })
      }
    }

    List(
      List(
        AuditReportConstants.API_AUDIT_API_NAME,
        AuditReportConstants.FILE_PATH_NAME,
        AuditReportConstants.AUDIT_LINE_NO
      )
    ) ++ workFlowResult.toList
  }
}
