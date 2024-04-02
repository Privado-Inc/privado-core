package ai.privado.rulevalidator

import ai.privado.entrypoint.CommandConstants
import ai.privado.model.Constants.{CONFIG_DIR_IN_CONFIG, PRETTY_LINE_SEPARATOR, RULES_DIR_IN_CONFIG}
import ai.privado.model.{CatLevelOne, ConfigRuleType}
import better.files.File
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.networknt.schema.{JsonSchema, JsonSchemaFactory, SpecVersion, ValidationMessage}
import org.slf4j.LoggerFactory

import java.util.regex.{Pattern, PatternSyntaxException}
import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters.SetHasAsScala

object YamlFileValidator {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val SCHEMA_DIR_PATH     = "/ai/privado/rulevalidator/schema/"
  private val JSON_SCHEMA_VERSION = SpecVersion.VersionFlag.V7
  private val NON_PATTERN_RULES =
    Set[String](
      CatLevelOne.THREATS.name,
      CatLevelOne.POLICIES.name,
      ConfigRuleType.SEMANTICS.toString,
      ConfigRuleType.SYSTEM_CONFIG.toString
    )
  private val SOURCES = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}sources.json")).mkString
  private val SINKS   = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}sinks.json")).mkString
  private val POLICIES =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}policies.json")).mkString
  private val THREATS = Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}threats.json")).mkString
  private val COLLECTIONS =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}collections.json")).mkString
  private val EXCLUSIONS =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}exclusions.json")).mkString
  private val SEMANTICS =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}semantics.json")).mkString
  private val SINK_SKIP_LIST =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}sinkSkipList.json")).mkString
  private val SYSTEM_CONFIG =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}systemConfig.json")).mkString
  private val AUDIT_CONFIG =
    Source.fromInputStream(getClass.getResourceAsStream(s"${SCHEMA_DIR_PATH}auditConfig.json")).mkString
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

    logger.trace(s"Validating directory : ${dir.pathAsString}")
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
        println("Validation")
        println(validateRuleFile(ruleFile, CommandConstants.VALIDATE) match {
          case Right(()) => None
          case Left(validationMessages: mutable.Set[String]) =>
            Some(ValidationFailure(validationMessages, ruleFile))
        })
        validateRuleFile(ruleFile, CommandConstants.VALIDATE) match {
          case Right(()) => None
          case Left(validationMessages: mutable.Set[String]) =>
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
    println(ruleFile.pathAsString)
    println(s"${configDirectory.pathAsString}/$RULES_DIR_IN_CONFIG")
    println(s"${configDirectory.pathAsString}/$CONFIG_DIR_IN_CONFIG")
    println(s"Some check which might be passing: ${!(ruleFile.pathAsString
        .contains(s"${configDirectory.pathAsString} / $RULES_DIR_IN_CONFIG") || ruleFile.pathAsString
        .contains(s"${configDirectory.pathAsString}/$CONFIG_DIR_IN_CONFIG"))}")
    if (
      !(ruleFile.pathAsString.contains(s"${configDirectory.pathAsString}/$RULES_DIR_IN_CONFIG") || ruleFile.pathAsString
        .contains(s"${configDirectory.pathAsString}/$CONFIG_DIR_IN_CONFIG"))
    ) {
      return false
    }
    validateRuleFile(ruleFile) match {
      case Right(()) => true
      case Left(validationMessages) =>
        if (validationMessages.nonEmpty) {
          validationMessages.foreach(vm => {
            println(vm)
            println(s"File ${ruleFile.pathAsString} has following problems, ignoring file ...")
            println(s"${vm}")
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
  def validateRuleFile(ruleFile: File, callerCommand: String = ""): Either[mutable.Set[String], Unit] = {
    println("validate")
    try {
      val yamlAsJson = mapper.readTree(ruleFile.contentAsString())
      matchSchemaFile(ruleFile, yamlAsJson, callerCommand) match {
        case Left(()) => Right(())
        case Right((typeOfRule: String, schemaFile: String)) =>
          if (NON_PATTERN_RULES.contains(typeOfRule)) {
            Left(ruleFile.validateJsonFile(schemaFile, yamlAsJson))
          } else {
            validateRegexPattern(yamlAsJson, typeOfRule) match {
              case Right(_)    => Left(ruleFile.validateJsonFile(schemaFile, yamlAsJson))
              case Left(value) => Left(value)
            }
          }
      }
    } catch {
      case e: Exception =>
        if (callerCommand == CommandConstants.VALIDATE) println(PRETTY_LINE_SEPARATOR)
        println(
          f"File : ${ruleFile.pathAsString}: Unable to parse file invalid syntax at [${e.toString.split(';').last}] Ignoring file ..."
        )
        Right(())
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
    *   Unit in case of no matching schema is found, else tuple of (RuleType: String, SchemaFilePath: String) for
    *   matching type
    */
  def matchSchemaFile(
    ruleFile: File,
    ruleJsonTree: JsonNode,
    callerCommand: String = ""
  ): Either[Unit, (String, String)] = {

    println(ruleFile)
    val catLevelOneKey =
      if (ruleJsonTree.fieldNames().hasNext) ruleJsonTree.fieldNames().next() else CatLevelOne.UNKNOWN.name
    logger.trace(s"Found CatLevelOne key '$catLevelOneKey' in file : ${ruleFile.pathAsString}")
    CatLevelOne
      .withNameWithDefault(catLevelOneKey) match {
      case CatLevelOne.SOURCES     => Right(CatLevelOne.SOURCES.name, SOURCES)
      case CatLevelOne.POLICIES    => Right(CatLevelOne.POLICIES.name, POLICIES)
      case CatLevelOne.THREATS     => Right(CatLevelOne.THREATS.name, THREATS)
      case CatLevelOne.COLLECTIONS => Right(CatLevelOne.COLLECTIONS.name, COLLECTIONS)
      case CatLevelOne.SINKS       => Right(CatLevelOne.SINKS.name, SINKS)
      case _ =>
        matchSchemaConfigFile(ruleFile, ruleJsonTree, callerCommand)
    }
  }

  def matchSchemaConfigFile(
    ruleFile: File,
    ruleJsonTree: JsonNode,
    callerCommand: String = ""
  ): Either[Unit, (String, String)] = {
    val configTypeKey =
      if (ruleJsonTree.fieldNames().hasNext) ruleJsonTree.fieldNames().next() else CatLevelOne.UNKNOWN.name
    ConfigRuleType.withNameDefaultHandler(configTypeKey) match {
      case ConfigRuleType.SEMANTICS      => Right(ConfigRuleType.SEMANTICS.toString, SEMANTICS)
      case ConfigRuleType.EXCLUSIONS     => Right(ConfigRuleType.EXCLUSIONS.toString, EXCLUSIONS)
      case ConfigRuleType.SINK_SKIP_LIST => Right(ConfigRuleType.SINK_SKIP_LIST.toString, SINK_SKIP_LIST)
      case ConfigRuleType.SYSTEM_CONFIG  => Right(ConfigRuleType.SYSTEM_CONFIG.toString, SYSTEM_CONFIG)
      case ConfigRuleType.AUDIT_CONFIG   => Right(ConfigRuleType.AUDIT_CONFIG.toString, AUDIT_CONFIG)

      case _ =>
        if (callerCommand == CommandConstants.VALIDATE) println(PRETTY_LINE_SEPARATOR)
        println(f"File : ${ruleFile.pathAsString}: Config Invalid. Correct the rules and try again.")
        Left(())
    }
  }

  def validateRegexPattern(content: JsonNode, typeOfRule: String): Either[mutable.Set[String], Unit] = {
    val contentJson = ujson.read(content.toString)(typeOfRule).arr.toList
    val errors      = mutable.Set[String]()
    contentJson.foreach(value => {
      value("patterns").arr.toList.foreach(pattern => {
        try {
          Pattern.compile(pattern.toString())
        } catch {
          case e: PatternSyntaxException =>
            errors.addOne(e.toString)
        }
      })
    })
    errors.size match {
      case 0 => Right(())
      case _ => Left(errors)
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
      *   a scala mutable set of String
      */
    def validateJsonFile(schemaFile: String, jsonObj: JsonNode): mutable.Set[String] = {
      val errors = mutable.Set[String]()
      file
        .loadSchema(schemaFile)
        .validate(jsonObj)
        .asScala
        .foreach(error => errors.addOne(error.toString))
      errors
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

case class ValidationFailure(validationMessages: mutable.Set[String], file: File)
