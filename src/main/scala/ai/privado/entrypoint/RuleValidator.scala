package ai.privado.entrypoint

import ai.privado.model.Constants.{CONFIG_DIR_IN_CONFIG, PRETTY_LINE_SEPARATOR, RULES_DIR_IN_CONFIG}
import ai.privado.rulevalidator.YamlFileValidator
import better.files.File
import org.slf4j.LoggerFactory

object RuleValidator extends CommandProcessor {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override var config: PrivadoInput = _

  override def process(): Either[String, Unit] = {
    println("Starting rule validations ...")
    validateRules() match {
      case Left(_) =>
        println("Error in parsing Rules")
        Left("")
      case Right(_) => validateConfig()
    }
  }

  def getRulesPathFromConfig: Either[Unit, File] = {
    if (config.externalConfigPath.nonEmpty) {
      Right(File(s"${config.externalConfigPath.head}/$RULES_DIR_IN_CONFIG"))
    } else {
      logger.error("Error while reading config path")
      Left(())
    }
  }

  def validateRules(): Either[String, Unit] = {
    getRulesPathFromConfig match {
      case Left(()) =>
        logger.error("Failed to validate rules directory")
        Left("")
      case Right(yamlDirectory) => validateDirectory(yamlDirectory)
    }
  }

  def validateConfig(): Either[String, Unit] = {
    getConfigPathFromConfig match {
      case Left(_) =>
        logger.error("Failed to validate rules directory")
        Left("")
      case Right(yamlDirectory) => validateDirectory(yamlDirectory)
    }
  }

  def getConfigPathFromConfig: Either[Unit, File] = {
    if (config.externalConfigPath.nonEmpty) {
      Right(File(s"${config.externalConfigPath.head}/$CONFIG_DIR_IN_CONFIG"))
    } else {
      logger.error("Error while reading config path")
      Left(())
    }
  }

  def validateDirectory(yamlDirectory: File): Either[String, Unit] = {
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
          println(s"Completed command: ${CommandConstants.VALIDATE} for directory: $yamlDirectory, with no errors")
        case 1 =>
          println(s"Completed command: ${CommandConstants.VALIDATE} for directory: $yamlDirectory, with 1 error")
        case _ =>
          println(
            s"Completed command: ${CommandConstants.VALIDATE} for directory: $yamlDirectory, with ${errorsFound.toString} errors"
          )
      }
      Right(())
    } catch {
      case ex: Exception =>
        println("Failed to validate rules", ex)
        logger.debug("Failed to validate rules", ex)
        Left(ex.toString)
    }
  }
}
