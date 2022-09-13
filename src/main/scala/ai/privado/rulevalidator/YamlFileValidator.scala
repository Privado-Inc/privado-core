package ai.privado.rulevalidator

import ai.privado.entrypoint.CommandConstants
import ai.privado.model.CatLevelOne
import better.files.File
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.networknt.schema.{JsonSchema, JsonSchemaFactory, SpecVersion, ValidationMessage}
import org.slf4j.LoggerFactory

import java.util
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.io.Source


object YamlFileValidator {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val PRETTY_LINE_SEPARATOR = "-"*100

  private val SCHEMA_DIR_PATH = "/ai/privado/rulevalidator/schema/"
  private val JSON_SCHEMA_VERSION = SpecVersion.VersionFlag.V7
  private val RULES_FOLDER_IN_CONFIG_DIR = "rules"

  private val SOURCES     = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}sources.json")).mkString
  private val SINKS       = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}sinks.json")).mkString
  private val POLICIES    = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}policies.json")).mkString
  private val THREATS     = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}threats.json")).mkString
  private val COLLECTIONS = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}collections.json")).mkString

  val mapper = new ObjectMapper(new YAMLFactory())

  val factory: JsonSchemaFactory = JsonSchemaFactory
    .builder(
      JsonSchemaFactory
        .getInstance(JSON_SCHEMA_VERSION)
    ).objectMapper(mapper)
    .build()

  implicit def betterFilesExtended(file: File): BetterFileUtil = new BetterFileUtil(file)
  implicit def jsonSchemaExtended(jsonSchema: JsonSchema): JsonSchemaExtension = new JsonSchemaExtension(jsonSchema)

  /**
    * Iterate recursively inside a directory path, filter for ".yaml" and ".yml" files
    * Identify corresponding schema file for each file and validate.
    * Filter all files to collect files with Validation Errors
    * @return an Iterator on ai.privado.rulevalidator.ValidationFailure objects
    */
  def validateDirectory(dir: File): Iterator[ValidationFailure] = {

    val validationErrors: Iterator[ValidationFailure] = dir
      .listRecursively
      .filter(
        subDir =>
          subDir
            .extension(toLowerCase=true)
            .toString
            .contains(".yaml")
            ||
            subDir
              .extension(toLowerCase=true)
              .toString
              .contains(".yml")
      )
      .map(
        ruleFile => {
          val yamlAsJson = mapper
            .readTree(
              ruleFile.contentAsString()
            )
          val schemaFile = matchSchemaFile(ruleFile, yamlAsJson, CommandConstants.VALIDATE)
          if (schemaFile != null)
            ValidationFailure(ruleFile.validateJsonFile(schemaFile, yamlAsJson), ruleFile)
          else null
        }
      ).filter(vf => vf != null)

    validationErrors
      .filter(
        ve =>
          !ve.validationMessages.isEmpty
      )
  }

  /**
    * Validate a single rule file.
    * Identify corresponding schema file for the input file and validate.
    * @param ruleFile better.files.File object of the rule file to be validated
    * @param configDirectory better.files.Files config location
    * @return Boolean stating whether the rule file is valid
    */
  def isValidRuleFile(ruleFile: File, configDirectory: File): Boolean = {
    if (!ruleFile.pathAsString.contains(s"${configDirectory.pathAsString}/$RULES_FOLDER_IN_CONFIG_DIR")) {
      return false
    }
    val yamlAsJson = mapper
      .readTree(
        ruleFile.contentAsString()
      )
    val schemaFileContent = matchSchemaFile(ruleFile, yamlAsJson)
    if (schemaFileContent == null)
      false
    else {
      val validationMessages = ruleFile.validateJsonFile(schemaFileContent, yamlAsJson).asScala
      if (validationMessages.nonEmpty) {
        validationMessages.foreach(vm => {
          println(s"File ${ruleFile.pathAsString} has following problems, ignoring file ...")
          println(s"${vm.getMessage}")
        })
        false
      }
      else true
    }
  }

  /**
    * Find appropriate schema file to validate the rule file against it
    * @param ruleFile better.files.File object for a YAML rule file to be validated
    * @param ruleJsonTree com.fasterxml.jackson.databind.JsonNode object containing json data of rule file
    *  @param callerCommand String value to govern pretty print of validation messages
    * @return an Iterator on ai.privado.rulevalidator.ValidationFailure objects
    */
  def matchSchemaFile(ruleFile: File, ruleJsonTree: JsonNode, callerCommand: String = ""): String = {

    val catLevelOneKey = if (ruleJsonTree.fieldNames().hasNext) ruleJsonTree.fieldNames().next() else CatLevelOne.UNKNOWN.name


    CatLevelOne
      .withNameWithDefault(
        catLevelOneKey
      ) match {
      case CatLevelOne.SOURCES => SOURCES
      case CatLevelOne.POLICIES => POLICIES
      case CatLevelOne.THREATS => THREATS
      case CatLevelOne.COLLECTIONS => COLLECTIONS
      case CatLevelOne.SINKS => SINKS
      case _ =>
        if (callerCommand == CommandConstants.VALIDATE) println(PRETTY_LINE_SEPARATOR)
        println(
          f"File : ${ruleFile.pathAsString} :Adding new rules under the category '$catLevelOneKey'" +
            f" is not supported. Ignoring file ...."
        )
//        if (callerCommand == CommandConstants.VALIDATE) println(PRETTY_LINE_SEPARATOR)
        null
    }
  }

  class BetterFileUtil(file: File) {

    /**
      * Generates a schema object using a json schema file
      * * @param schemaFile better.files.File schema file to validate json
      * @return an instance of com.networknt.schema.JsonSchema
      */
    def loadSchema(schemaFile: String): JsonSchema = {
      factory
        .getSchema(
          schemaFile
        )
    }

    /**
      * Validate the implicitly inferred rule file against which this method is invoked
      * @param schemaFile String content of the schema file used to validate json
      * @return a java set of com.networknt.schema.ValidationMessage
      */
    def validateJsonFile(schemaFile: String, jsonObj: JsonNode): util.Set[ValidationMessage] = {
      file
        .loadSchema(schemaFile)
        .validate(jsonObj)
    }

  }

  class JsonSchemaExtension(jsonSchema: JsonSchema) {

    /**
      * Validate the input param file using an implicitly inferred json-schema file
      * against which this method is invoked
      * @param jsonFile com.fasterxml.jackson.databind.JsonNode JsonContent to validate
      * @return a java set of com.networknt.schema.ValidationMessage
      */
    def validate(jsonFile: JsonNode): util.Set[ValidationMessage] = {
      jsonSchema.
        validate(
          jsonFile
        )
    }

  }

}

case class ValidationFailure(validationMessages: util.Set[ValidationMessage], file: File)
