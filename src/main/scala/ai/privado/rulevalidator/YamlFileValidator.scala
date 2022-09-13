package ai.privado.rulevalidator

import ai.privado.entrypoint.CommandConstants
import ai.privado.model.CatLevelOne
import ai.privado.model.Constants.{PRETTY_LINE_SEPARATOR, RULES_DIR_IN_CONFIG}
import better.files.File
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.networknt.schema.{JsonSchema, JsonSchemaFactory, SpecVersion, ValidationMessage}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.io.Source

object YamlFileValidator {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val SCHEMA_DIR_PATH     = "/ai/privado/rulevalidator/schema/"
  private val JSON_SCHEMA_VERSION = SpecVersion.VersionFlag.V7

  private val SOURCES = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}sources.json")).mkString
  private val SINKS   = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}sinks.json")).mkString
  private val POLICIES =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}policies.json")).mkString
  private val THREATS = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}threats.json")).mkString
  private val COLLECTIONS =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}collections.json")).mkString

  val mapper = new ObjectMapper(new YAMLFactory())

  val factory: JsonSchemaFactory = JsonSchemaFactory
    .builder(
      JsonSchemaFactory
        .getInstance(JSON_SCHEMA_VERSION)
    )
    .objectMapper(mapper)
    .build()

  implicit def betterFilesExtended(file: File): BetterFileUtil                 = new BetterFileUtil(file)
  implicit def jsonSchemaExtended(jsonSchema: JsonSchema): JsonSchemaExtension = new JsonSchemaExtension(jsonSchema)

  /** Iterate recursively inside a directory path, filter for ".yaml" and ".yml" files Identify corresponding schema
    * file for each file and validate. Filter all files to collect files with Validation Errors
    * @param dir
    *   rules directory containing files to be validated
    * @return
    *   an Iterator on ai.privado.rulevalidator.ValidationFailure objects
    */
  def validateDirectory(dir: File): Iterator[ValidationFailure] = {

    logger.debug(s"Validating directory : ${dir.pathAsString}")
    val validationErrors: Iterator[ValidationFailure] = dir.listRecursively
      .filter(subDir =>
        subDir
          .extension(toLowerCase = true)
          .toString
          .contains(".yaml")
          ||
            subDir
              .extension(toLowerCase = true)
              .toString
              .contains(".yml")
      )
      .flatMap(ruleFile => {
        validateRuleFile(ruleFile, CommandConstants.VALIDATE) match {
          case Left(()) => None
          case Right(validationMessages: mutable.Set[ValidationMessage]) =>
            Some(ValidationFailure(validationMessages, ruleFile))
        }
      })
      .filter(_.validationMessages.nonEmpty)
    validationErrors
  }

  /** Validate a single rule file.
    * @param ruleFile
    *   better.files.File object of the rule file to be validated
    * @param configDirectory
    *   better.files.Files config location
    * @return
    *   Boolean stating whether the rule file is valid
    */
  def isValidRuleFile(ruleFile: File, configDirectory: File): Boolean = {
    if (!ruleFile.pathAsString.contains(s"${configDirectory.pathAsString}/$RULES_DIR_IN_CONFIG")) {
      return false
    }
    validateRuleFile(ruleFile) match {
      case Left(()) => false
      case Right(validationMessages) =>
        if (validationMessages.nonEmpty) {
          validationMessages.foreach(vm => {
            println(s"File ${ruleFile.pathAsString} has following problems, ignoring file ...")
            println(s"${vm.getMessage}")
          })
          false
        } else true
    }
  }

  /** Validate a single rule file. Identify corresponding schema file for the input file and validate.
    * @param ruleFile
    *   better.files.File object of the rule file to be validated
    * @param callerCommand
    *   String value to govern pretty print of validation messages
    * @return
    *   scala mutable Set of ValidationMessage in case of validation errors, None if no errors are found
    */
  def validateRuleFile(ruleFile: File, callerCommand: String = ""): Either[Unit, mutable.Set[ValidationMessage]] = {
    val yamlAsJson = mapper.readTree(ruleFile.contentAsString())
    matchSchemaFile(ruleFile, yamlAsJson, callerCommand) match {
      case Left(()) => Left(())
      case Right(schemaFile: String) =>
        Right(ruleFile.validateJsonFile(schemaFile, yamlAsJson))
    }
  }

  /** Find appropriate schema file to validate the rule file against it
    * @param ruleFile
    *   better.files.File object for a YAML rule file to be validated
    * @param ruleJsonTree
    *   com.fasterxml.jackson.databind.JsonNode object containing json data of rule file
    * @param callerCommand
    *   String value to govern pretty print of validation messages
    * @return
    *   Unit in case of no matching schema is found, else String content of matched schema file
    */
  def matchSchemaFile(ruleFile: File, ruleJsonTree: JsonNode, callerCommand: String = ""): Either[Unit, String] = {

    val catLevelOneKey =
      if (ruleJsonTree.fieldNames().hasNext) ruleJsonTree.fieldNames().next() else CatLevelOne.UNKNOWN.name
    logger.debug(s"Found CatLevelOne key '$catLevelOneKey' in file : ${ruleFile.pathAsString}")
    CatLevelOne
      .withNameWithDefault(catLevelOneKey) match {
      case CatLevelOne.SOURCES     => Right(SOURCES)
      case CatLevelOne.POLICIES    => Right(POLICIES)
      case CatLevelOne.THREATS     => Right(THREATS)
      case CatLevelOne.COLLECTIONS => Right(COLLECTIONS)
      case CatLevelOne.SINKS       => Right(SINKS)
      case _ =>
        if (callerCommand == CommandConstants.VALIDATE) println(PRETTY_LINE_SEPARATOR)
        println(
          f"File : ${ruleFile.pathAsString} :Adding new rules under the category '$catLevelOneKey'" +
            f" is not supported. Ignoring file ...."
        )
        Left(())
    }
  }

  class BetterFileUtil(file: File) {

    /** Generates a schema object using a json schema file * @param schemaFile better.files.File schema file to validate
      * json
      * @return
      *   an instance of com.networknt.schema.JsonSchema
      */
    def loadSchema(schemaFile: String): JsonSchema = {
      factory
        .getSchema(schemaFile)
    }

    /** Validate the implicitly inferred rule file against which this method is invoked
      * @param schemaFile
      *   String content of the schema file used to validate json
      * @param jsonObj
      *   JsonNode object -> Yaml file converted to Json tree.
      * @return
      *   a scala mutable set of com.networknt.schema.ValidationMessage
      */
    def validateJsonFile(schemaFile: String, jsonObj: JsonNode): mutable.Set[ValidationMessage] = {
      file
        .loadSchema(schemaFile)
        .validate(jsonObj).asScala
    }

  }

  class JsonSchemaExtension(jsonSchema: JsonSchema) {

    /** Validate the input param file using an implicitly inferred json-schema file against which this method is invoked
      * @param jsonFile
      *   com.fasterxml.jackson.databind.JsonNode JsonContent to validate
      * @return
      *   a scala mutable set of com.networknt.schema.ValidationMessage
      */
    def validate(jsonFile: JsonNode): mutable.Set[ValidationMessage] = {
      jsonSchema.validate(jsonFile).asScala
    }

  }

}

case class ValidationFailure(validationMessages: mutable.Set[ValidationMessage], file: File)
