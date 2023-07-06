package ai.privado.entrypoint

import ai.privado.model.Constants.{CONFIG_DIR_IN_CONFIG, PRETTY_LINE_SEPARATOR, RULES_DIR_IN_CONFIG}
import ai.privado.rulevalidator.YamlFileValidator
import better.files.File
import org.slf4j.LoggerFactory

object RuleValidator extends CommandProcessor {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def process(): Either[String, Unit] = {
    println("Starting rule validations ...")
    validateRules() match {
      case Some(returnVal) =>
        println(returnVal)
        validateConfig() match {
          case Some(value) => Right(println(value))
          case _           => Left("Error in parsing Config rules")
        }
      case _ => Left("Error in parsing Rules")
    }
  }

  def getRulesPathFromConfig: Option[File] = {
    if (config.externalConfigPath.nonEmpty) {
      Some(File(s"${config.externalConfigPath.head}/$RULES_DIR_IN_CONFIG"))
    } else {
      logger.error("Error while reading config path")
      None
    }
  }

  def validateRules(): Option[String] = {
    getRulesPathFromConfig match {
      case Some(yamlDirectory) => Some(validateDirectory(yamlDirectory))
      case _ =>
        logger.error("Failed to validate rules directory")
        None
    }
  }

  def validateConfig(): Option[String] = {
    getConfigPathFromConfig match {
      case Some(yamlDirectory) => Some(validateDirectory(yamlDirectory))
      case _ =>
        logger.error("Failed to validate rules directory")
        None
    }
  }

  def getConfigPathFromConfig: Option[File] = {
    if (config.externalConfigPath.nonEmpty) {
      Some(File(s"${config.externalConfigPath.head}/$CONFIG_DIR_IN_CONFIG"))
    } else {
      logger.error("Error while reading config path")
      None
    }
  }

  def validateDirectory(yamlDirectory: File): String = {
    try {
      val validationFailures = YamlFileValidator.validateDirectory(yamlDirectory)
      var errorsFound        = 0
      validationFailures.toList.foreach(vf => {
        errorsFound += 1
        println(PRETTY_LINE_SEPARATOR)
        println(f"File: ${vf.file.pathAsString} has following errors:")
        vf.validationMessages.foreach(msg => println(msg))
      })
      println(PRETTY_LINE_SEPARATOR)
      errorsFound match {
        case 0 =>
          s"Completed command: ${CommandConstants.VALIDATE} for directory: $yamlDirectory, with no errors"
        case 1 =>
          s"Completed command: ${CommandConstants.VALIDATE} for directory: $yamlDirectory, with 1 error"
        case _ =>
          s"Completed command: ${CommandConstants.VALIDATE} for directory: $yamlDirectory, with ${errorsFound.toString} errors"
      }
    } catch {
      case ex: Exception =>
        logger.debug("Failed to validate rules", ex)
        s"Failed to validate rules ${ex.getMessage}"
    }
  }
}
