package ai.privado.passes

import ai.privado.utility.Utilities
import ai.privado.cache.{DatabaseDetailsCache, RuleCache}
import io.joern.x2cpg.SourceFiles
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.sql.{SQLColumn, SQLQuery}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.SQLParser
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewJavaProperty
import overflowdb.BatchedUpdate

import java.io.File
import scala.io.Source
import java.util.Properties
import scala.jdk.CollectionConverters.*
import io.circe.parser.*
import io.circe.*

import scala.collection.mutable
import com.typesafe.config.*

import scala.xml.XML
import com.github.wnameless.json.flattener.JsonFlattener
import io.circe.yaml.parser
import org.yaml.snakeyaml.{LoaderOptions, Yaml}
import org.yaml.snakeyaml.nodes.{MappingNode, Node, NodeTuple, ScalarNode, SequenceNode}

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import ai.privado.model._
import ai.privado.tagger.PrivadoParallelCpgPass
import com.github.javaparser.utils.ProjectRoot
import org.yaml.snakeyaml.constructor.SafeConstructor

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import java.nio.file.{Files, Path, Paths}
import scala.util.control.Breaks.*
import scala.util.matching.Regex

object FileExtensions {
  val YAML = ".yaml"
  val YML  = ".yml"
}

class DBTParserPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {

  val logger          = LoggerFactory.getLogger(getClass)
  val DEFAULT_DB_NAME = "DBT"
  val CONFIG_NAME_KEY = "name"
  val CONFIG_DESC_KEY = "description"

  override def generateParts(): Array[String] = {
    getConfigFiles(
      projectRoot,
      Set(FileExtensions.YAML, FileExtensions.YML),
      Set("dbt_project[.]yaml", "dbt_project[.]yml")
    ).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, projectFile: String): Unit = {
    logger.debug(f"Processing DBT Project: $projectFile")
    getYAML(projectFile) match {
      case Success(projectData) => {
        val projectName         = projectData.get(CONFIG_NAME_KEY).asInstanceOf[String]
        val profileName         = projectData.get("profile").asInstanceOf[String]
        val modelsDirectoryName = getModelsDirectoryName(projectData)

        // extract db information from profile
        val (profileFile, dbKey, dbName, dbHost, dbPlatform) = getDatabaseInfo(projectFile, profileName) match {
          case Success(value) => value
          case Failure(err) =>
            logger.error(f"error while getting database info: ${projectFile}: ${err}")
            ("", "", DEFAULT_DB_NAME, "", "")
        }

        // get all models from the model directory
        val models = getModels(projectFile, modelsDirectoryName) match {
          case Success(value) => value
          case Failure(err) =>
            logger.error(f"error while getting models for profile: ${projectFile}: ${err}")
            Array[java.util.Map[String, Any]]()
        }

        // transform models into expected schema format
        val schema = transformModelsToSchema(projectName, dbPlatform, models)
        // create rule and add metadata for dbt storage sink
        val ruleInfo = createDatabaseSinkRule(projectName, dbName, dbPlatform, dbHost, schema)

        // create and tag database node
        val dbNode = createAndTagDatabaseNode(builder, ruleInfo, projectFile, projectName, profileFile, dbName, dbKey)
        // create sql query node
        val sqlNode = createSQLQueryNode(builder, dbNode)

        // create table and column nodes
        models.zipWithIndex.foreach { case (model, i) =>
          val tableNode = createTableColumnNodesFromModels(builder, model, i)
          builder.addEdge(sqlNode, tableNode, EdgeTypes.AST)
        }

        logger.debug(
          f"DBT Pass Detection: ruleId: ${ruleInfo.id}, projectName=${projectName}, profileName=${profileName}, " +
            f"models=${models.length}, dbName=$dbName, dbHost=$dbHost, dbPlatform=$dbPlatform, dbKey: $dbKey, " +
            f"profileFile=$profileFile"
        )
      }
      case Failure(e) => {
        logger.error(f"error while processing YAML file: ${projectFile}: ${e}")
        None
      }
    }
  }

  private def transformModelsToSchema(
    projectName: String,
    dbPlatform: String,
    models: Array[java.util.Map[String, Any]]
  ): DatabaseSchema = {
    val tables = models.map { table =>
      val columns = getColumnsFromTableModel(table).asScala.map { col =>
        DatabaseColumn(
          col.get(CONFIG_NAME_KEY).asInstanceOf[String],
          col.getOrDefault(CONFIG_DESC_KEY, "").asInstanceOf[String],
          "",
          findMatchingSourceId(col.get(CONFIG_NAME_KEY).asInstanceOf[String])
        )
      }.toList

      DatabaseTable(
        table.get(CONFIG_NAME_KEY).asInstanceOf[String],
        table.getOrDefault(CONFIG_DESC_KEY, "").asInstanceOf[String],
        columns
      )
    }.toList

    val dbSchema = DatabaseSchema(kind = "dbt", projectName = projectName, platform = dbPlatform, tables = tables)

    dbSchema
  }

  private def createDatabaseSinkRule(
    projectName: String,
    dbName: String,
    dbPlatform: String,
    dbHost: String,
    schema: DatabaseSchema
  ) = {
    val ruleId   = f"Storages.DBT.ReadAndWrite.${projectName}"
    val ruleHost = if (dbHost != "") dbHost else "getdbt.com"

    val customDatabaseSinkRule = RuleInfo(
      ruleId,
      f"${dbName}",
      "",
      Array[String](ruleHost),
      List[String](),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.DEFAULT,
      Array[String]()
    )

    val dbDetails = DatabaseDetails(dbName, dbPlatform, dbHost, "", "", Some(schema))

    ruleCache.setRuleInfo(customDatabaseSinkRule)
    DatabaseDetailsCache.addDatabaseDetails(dbDetails, ruleId)

    customDatabaseSinkRule
  }

  private def findMatchingSourceId(query: String) = {
    val matchingRules = for {
      rule <- ruleCache.getRule.sources
      if query.matches(rule.combinedRulePattern)
    } yield rule

    if (!matchingRules.isEmpty) matchingRules.head.id else ""
  }

  private def createSQLQueryNode(builder: DiffGraphBuilder, dbNode: NewDbNode) = {
    val sqlNode = NewSqlQueryNode().name(DEFAULT_DB_NAME).code(DEFAULT_DB_NAME).order(0)
    builder.addNode(sqlNode)
    builder.addEdge(dbNode, sqlNode, EdgeTypes.AST)

    sqlNode
  }
  private def createAndTagDatabaseNode(
    builder: DiffGraphBuilder,
    ruleInfo: RuleInfo,
    projectFile: String,
    projectName: String,
    profileFile: String,
    dbName: String,
    dbKey: String
  ) = {
    val (fileName, (lineNumber, columnNumber, matchedLine)) = dbName match {
      case DEFAULT_DB_NAME => (projectFile, findFirstPatternInYAMLFile(projectFile, CONFIG_NAME_KEY, projectName))
      case _               => (profileFile, findFirstPatternInYAMLFile(profileFile, dbKey, dbName))
    }

    val fileNode = addFileNode(fileName, builder)

    val databaseNode = NewDbNode()
      .name(dbName)
      .code(matchedLine)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .order(0)

    builder.addNode(databaseNode)
    builder.addEdge(databaseNode, fileNode, EdgeTypes.SOURCE_FILE)
    addRuleTags(builder, databaseNode, ruleInfo)

    databaseNode
  }

  private def addRuleTags(builder: DiffGraphBuilder, databaseNode: AstNodeNew, ruleInfo: RuleInfo) = {
    def storeForTag(builder: DiffGraphBuilder, node: AstNodeNew, tagName: String, tagValue: String = "") = {
      builder.addEdge(node, NewTag().name(tagName).value(tagValue), EdgeTypes.TAGGED_BY)
      builder
    }

    storeForTag(builder, databaseNode, Constants.id, ruleInfo.id)
    storeForTag(builder, databaseNode, Constants.nodeType, ruleInfo.nodeType.toString)
    storeForTag(builder, databaseNode, Constants.catLevelOne, ruleInfo.catLevelOne.name)
    storeForTag(builder, databaseNode, Constants.catLevelTwo, ruleInfo.catLevelTwo)
  }

  private def createTableColumnNodesFromModels(
    builder: DiffGraphBuilder,
    model: java.util.Map[String, Any],
    tableQueryOrder: Int
  ) = {
    val modelFile = model.get("filePath").asInstanceOf[String]
    val tableName = model.get(CONFIG_NAME_KEY).asInstanceOf[String]

    // create file node
    val fileNode = addFileNode(modelFile, builder)

    // create table node
    val (tableLineNumber, tableColumnNumber, tableMatchedLine) =
      findFirstPatternInYAMLFile(modelFile, CONFIG_NAME_KEY, tableName)
    val tableNode = NewSqlTableNode()
      .name(tableName)
      .code(tableMatchedLine)
      .lineNumber(tableLineNumber)
      .columnNumber(tableColumnNumber)
      .order(tableQueryOrder)

    logger.debug(f"Creating table node for $tableName")

    builder.addNode(tableNode)
    builder.addEdge(tableNode, fileNode, EdgeTypes.SOURCE_FILE)

    // create column nodes
    val columns = getColumnsFromTableModel(model)
    columns.asScala.zipWithIndex.foreach { case (column, index) =>
      val colName = column.get(CONFIG_NAME_KEY).asInstanceOf[String]
      val (colLineNumber, colColumnNumber, colMatchedLine) =
        findFirstPatternInYAMLFile(modelFile, CONFIG_NAME_KEY, colName)
      val columnNode = NewSqlColumnNode()
        .name(colName)
        .code(colMatchedLine)
        .lineNumber(colLineNumber)
        .columnNumber(colColumnNumber)
        .order(index)

      logger.debug(f"Creating column node for $colName in table node $tableName")

      // link nodes
      builder.addNode(columnNode)
      builder.addEdge(tableNode, columnNode, EdgeTypes.AST)
      builder.addEdge(columnNode, fileNode, EdgeTypes.SOURCE_FILE)
    }

    tableNode
  }

  private def findFirstPatternInYAMLFile(filePath: String, key: String, value: String): (Int, Int, String) = {
    val (escapedKey, escapedValue) = (Regex.quote(key), Regex.quote(value))
    val pattern                    = s"""["']?$escapedKey["']?\\s*:\\s*["']?$escapedValue["']?""".r
    val lines                      = Files.readAllLines(Paths.get(filePath)).toArray(Array.ofDim[String](0))

    val (defaultRow, defaultCol) = (1, -1)

    lines.zipWithIndex
      .flatMap { case (line, rowIndex) =>
        val matches = pattern.findFirstMatchIn(line)
        matches.map(matchResult => (rowIndex + 1, matchResult.start + 1, matchResult.group(0)))
      }
      .headOption
      .getOrElse((defaultRow, defaultCol, ""))
  }

  private def getColumnsFromTableModel(model: java.util.Map[String, Any]) = {
    val emptyDefault = java.util.ArrayList[java.util.Map[String, Any]]
    val columns      = model.getOrDefault("columns", emptyDefault)
    columns match {
      case null => emptyDefault // when key exists without any value
      case cols =>
        cols.asInstanceOf[java.util.List[java.util.Map[String, Any]]] // for all other cases including default
    }
  }
  private def getModels(dbtProjectFile: String, modelsDirectoryName: String) = Try {
    val dbtProjectRoot  = getDirectoryName(dbtProjectFile)
    val modelsDirectory = Paths.get(dbtProjectRoot, modelsDirectoryName).toString
    val modelsFiles     = getConfigFiles(modelsDirectory, Set(FileExtensions.YAML, FileExtensions.YML), Set()).toArray

    val modelTables = ArrayBuffer[java.util.Map[String, Any]]()
    modelsFiles.foreach { modelFile =>
      getYAML(modelFile) match {
        case Success(data) =>
          if (data != null && !data.isEmpty && data.containsKey("models")) {
            val models = data.get("models").asInstanceOf[java.util.List[java.util.Map[String, Any]]]
            models.forEach(x =>
              if (x.containsKey(CONFIG_NAME_KEY)) {
                x.put("filePath", modelFile)
                modelTables += x
              }
            )
          }
        case Failure(e) =>
          logger.error(f"error while processing models YAML file: ${modelFile}: ${e}")
      }
    }

    modelTables.toArray
  }

  private def getDirectoryName(path: String): String = {
    val file   = new File(path)
    val parent = file.getParent
    if (parent == null) ""
    else parent
  }

  private def getDatabaseInfo(dbtProjectFile: String, dbtProfileName: String) = Try {
    var result = ("", "", DEFAULT_DB_NAME, "", "")

    if (dbtProfileName != "null") {
      val dbtProjectRoot = getDirectoryName(dbtProjectFile)
      val profilesFiles = getConfigFiles(
        dbtProjectRoot,
        Set(FileExtensions.YAML, FileExtensions.YML),
        Set("profiles[.]yaml", "profiles[.]yml")
      ).toArray

      for (profileFile <- profilesFiles if result._3 == DEFAULT_DB_NAME) {
        val profileData = getYAML(profileFile) match {
          case Success(profileData) =>
            profileData.get(dbtProfileName) match {
              case null  => null
              case value => value.asInstanceOf[java.util.Map[String, Any]]
            }
          case Failure(e) =>
            logger.error(f"error while processing profile YAML file: ${profileFile}: ${e}")
            null
        }

        if (profileData != null) {
          val defaultOutput = profileData.get("target").asInstanceOf[String]
          if (profileData.containsKey("outputs")) {
            val outputs = profileData.get("outputs").asInstanceOf[java.util.Map[String, Any]]
            getProfileOutputData(outputs, defaultOutput) match {
              case Some((outputKey, outputData)) =>
                val dbKeys = Array("project", "dataset", "dbname", "database", "schema", "type")
                breakable {
                  dbKeys.foreach { k =>
                    if (outputData.containsKey(k)) {
                      result = (
                        profileFile,
                        k,
                        outputData.get(k).asInstanceOf[String],
                        outputData.getOrDefault("host", "").asInstanceOf[String],
                        outputData.getOrDefault("type", "").asInstanceOf[String]
                      )
                      break
                    }
                  }
                }

              case _ =>
                logger.error(f"could not find any output data in profiles: ${profileFile}")
            }
          }
        }
      }
    }
    result
  }

  private def getProfileOutputData(data: java.util.Map[String, Any], defaultOutput: String) = {
    // look in production outputs
    var result: Option[(String, java.util.Map[String, Any])] = None

    val productionKeys = Array("production", "prod", "prd")
    productionKeys.foreach(x => {
      if (result.isEmpty && data.containsKey(x)) {
        result = Some(x, data.get(x).asInstanceOf[java.util.Map[String, Any]])
      }
    })

    // look in default output
    if (result.isEmpty && data.containsKey(defaultOutput)) {
      result = Some(defaultOutput, data.get(defaultOutput).asInstanceOf[java.util.Map[String, Any]])
    }

    // look in first (or any)
    if (result.isEmpty && !data.isEmpty) {
      val headKeyValue = data.entrySet().iterator().next()
      result = Some(headKeyValue.getKey, headKeyValue.getValue.asInstanceOf[java.util.Map[String, Any]])
    }

    result
  }
  private def getModelsDirectoryName(data: java.util.Map[String, Any]) = {
    data.get("model-paths") match {
      case value: java.util.List[_] if !value.isEmpty =>
        value.get(0).toString
      case value: String =>
        value
      case _ =>
        "models"
    }
  }

  private def getYAML(file: String) = Try {
    val yamlContent = better.files.File(file).contentAsString // Read the YAML file content as a string
    val yaml        = new Yaml(new SafeConstructor(LoaderOptions()))
    yaml.load(yamlContent).asInstanceOf[java.util.Map[String, Any]]
  }

  private def getConfigFiles(
    projectRoot: String,
    extensions: Set[String],
    allowedFiles: Set[String] = Set()
  ): List[String] = {
    SourceFiles
      .determine(Set(projectRoot), extensions)
      .filter(_.matches(f".*(${allowedFiles.mkString("|")})"))
      .filter(file => Utilities.isFileProcessable(file, ruleCache))
      .distinct
  }

  private def addFileNode(file: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val rootPath: Path     = Paths.get(projectRoot)
    val filePath: Path     = Paths.get(file)
    val relativePath: Path = rootPath.relativize(filePath)

    val fileNode = NewFile().name(relativePath.toString)
    builder.addNode(fileNode)
    fileNode
  }
}
