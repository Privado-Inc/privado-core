package ai.privado.inputprocessor

import org.slf4j.LoggerFactory
import upickle.default.*

import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try}

case class DependencyInfo(
  groupId: String,
  dependencyName: String,
  version: String,
  code: String,
  ruleId: String,
  ruleName: String,
  ruleDomains: List[String],
  ruleTags: List[String],
  lineNumber: Int,
  filePath: String
) {
  def getFullDependencyName(): String = {
    if groupId.isEmpty then dependencyName else s"$groupId.$dependencyName"
  }
}

object DependencyInfo {
  implicit val reader: Reader[DependencyInfo] = macroR[DependencyInfo]
}
trait DependencyTaggingProcessor {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def parseDependencyInfo(filePath: String): List[DependencyInfo] = {
    // Manually provide a reader for List[DependencyInfo]
    Try(read[List[DependencyInfo]](new String(Files.readAllBytes(Paths.get(filePath))))) match {
      case Success(dependencies) => dependencies
      case Failure(exception) =>
        logger.error(s"Error while parsing $filePath : ", exception)
        List()
    }
  }
}
