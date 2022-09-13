package ai.privado.entrypoint

import ai.privado.rulevalidator.YamlFileValidator
import better.files.File
import org.slf4j.LoggerFactory

object RuleValidator extends CommandProcessor {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val RULES_DIR_IN_CONFIG = "rules"

  override var config: PrivadoInput = _

  override def process(): Either[String, Unit] = {
    println("Starting rule validations ...")
    validateRules()
  }

  def getRulesPathFromConfig: Either[Unit, File] = {
    if (config.externalConfigPath.nonEmpty) {
      Right(File(s"${config.externalConfigPath.head}/$RULES_DIR_IN_CONFIG"))
    }
    else {
      logger.error("Error while reading config path")
      Left(())
    }
  }

  def validateRules(): Either[String, Unit] = {

    getRulesPathFromConfig match {
      case Left(()) =>
        logger.error("Failed to validate rules directory")
        Left("")
      case Right(yamlDirectory) =>
        try {
          val validationFailures = YamlFileValidator.validateDirectory(yamlDirectory)
          var errorsFound = 0
          validationFailures.toList.foreach(
            vf => {
              errorsFound += 1
              println("---------------------------------------------------------------------------------------------------")
              println(f"File: ${vf.file.pathAsString} has following errors:")
              vf.validationMessages.forEach(msg => println(msg))
            }
          )
          println("---------------------------------------------------------------------------------------------------")
          errorsFound match {
            case 0 => println(s"Completed command: validate-rule-set for directory: $yamlDirectory, with no errors")
            case 1 => println(
              s"Completed command: validate-rule-set for directory: $yamlDirectory, with ${errorsFound.toString} error"
            )
            case _ => println(
              s"Completed command: validate-rule-set for directory: $yamlDirectory, with ${errorsFound.toString} errors"
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
}

