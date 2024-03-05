package ai.privado.audit

import io.shiftleft.codepropertygraph.generated.Cpg

import ai.privado.languageEngine.java.language.NodeStarters
import scala.collection.mutable.ListBuffer
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.util.{Success, Try}

object LiteralReport {

  private val logger = LoggerFactory.getLogger(getClass)

  def processURLAudit(xtocpg: Try[Cpg]): List[List[String]] = {
    processRegexAuditResult(xtocpg, AuditReportConstants.URL_REGEX, AuditReportConstants.URL_AUDIT_URL_NAME)
  }

  def processHTTPAudit(xtocpg: Try[Cpg]): List[List[String]] = {
    processRegexAuditResult(xtocpg, AuditReportConstants.HTTP_REGEX, AuditReportConstants.HTTP_AUDIT_HTTP_NAME)
  }

  private def processRegexAuditResult(
    xtocpg: Try[Cpg],
    regexPattern: String,
    propertyName: String
  ): List[List[String]] = {
    val workFlowResult = new ListBuffer[List[String]]()
    xtocpg match {
      case Success(cpg) => {
        val codeURLList       = cpg.literal.code(regexPattern).l
        val propertiesURLList = cpg.property.l.filter(pair => pair.value.matches(regexPattern))
        codeURLList.foreach(literal => {
          workFlowResult += List(literal.code, literal.file.head.name, literal.lineNumber.getOrElse(0).toString)
        })
        propertiesURLList.foreach(property => {
          workFlowResult += List(property.value, property.file.head.name, property.lineNumber.getOrElse(0).toString)
        })

        List(
          List(propertyName, AuditReportConstants.FILE_PATH_NAME, AuditReportConstants.AUDIT_LINE_NO)
        ) ++ workFlowResult.toList
      }
    }
  }
}
