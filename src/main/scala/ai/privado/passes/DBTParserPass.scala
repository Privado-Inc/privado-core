package ai.privado.passes

import ai.privado.utility.Utilities
import ai.privado.cache.RuleCache
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

import java.io.{File}
import scala.io.{Source}
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

import scala.jdk.CollectionConverters.*
import ai.privado.model.Language
import ai.privado.tagger.PrivadoParallelCpgPass
import org.yaml.snakeyaml.constructor.SafeConstructor

import scala.collection.mutable.ListBuffer
import scala.util.{Try, Success, Failure}


object FileExtensions {
  val YAML       = ".yaml"
  val YML        = ".yml"
}

class DBTParserPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[String](cpg) {

  val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] = {
    getConfigFiles(
      projectRoot,
      Set(FileExtensions.YAML, FileExtensions.YML),
      Set("dbt_project[.]yaml", "dbt_project[.]yml")
    ).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, projectFile: String): Unit = {
    // Parse project YAML and get metadata from projectFile
    // Get models directory
    // parse all yamls in models directory: check for structure of file if it represents model / table / columns
    // populate that data in nodes

    logger.info(f">>>>>> wooohoooo: file: $projectFile")
    getYAML(projectFile) match {
      case Success(projectData) => {
        val projectName = projectData.get("name").asInstanceOf[String]
        val profileName = projectData.get("profile").asInstanceOf[String]
        val modelsDirectoryName = getModelsDirectoryName(projectData)
        val (dbName, dbHost, dbPlatform) = getDatabaseInfo(projectFile, profileName) match {
          case Success(value) => value
          case Failure(err) =>
            logger.error(f"error while getting database info: ${projectFile}: ${err}")
            ("DBT", "", "")
        }

        logger.info(f">>>>>> DBT DBT: projectName=${projectName}, profileName=${profileName}, modelsDirectoryName=${modelsDirectoryName}, dbName=$dbName, dbHost=$dbHost, dbPlatform=$dbPlatform")

      }
      case Failure(e) => {
        logger.error(f"error while processing YAML file: ${projectFile}: ${e}")
        None
      }
    }

  }

  private def getDirectoryName(path: String): String = {
    val file = new File(path)
    val parent = file.getParent
    if (parent == null) ""
    else parent
  }

  private def getDatabaseInfo(dbtProjectFile: String, dbtProfileName: String) = Try {
    val result = ("DBT", "", "")

    if (dbtProfileName != "null") {
      val dbtProjectRoot = getDirectoryName(dbtProjectFile)
      val profilesFiles = getConfigFiles(
        dbtProjectRoot,
        Set(FileExtensions.YAML, FileExtensions.YML),
        Set("profiles[.]yaml", "profiles[.]yml")
      ).toArray

      for ( profileFile <- profilesFiles if result._1 == "DBT" ) {
        val profileData = getYAML(profileFile) match {
          case Success(profileData) =>
            profileData.get(dbtProfileName) match {
              case null => null
              case value => value.asInstanceOf[java.util.Map[String, Any]]
            }
          case Failure(e) =>
            logger.error(f"error while processing profile YAML file: ${profileFile}: ${e}")
            null
        }

        if (profileData != null) {
          logger.info(f">>>>>>>>>?>> Hey got some profile data for ${profileFile}")
          val defaultOutput = profileData.get("target").asInstanceOf[String]
          if (profileData.containsKey("outputs")) {
            val outputs = profileData.get("outputs").asInstanceOf[java.util.Map[String, Any]]
            getProfileOutputData(outputs, defaultOutput) match {
              case Some((outputKey, outputData)) =>
                logger.info(f"hehllo outputKey: ${outputKey}")

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
        logger.info(f"PROD CASE: ")
        result = Some(x, data.get(x).asInstanceOf[java.util.Map[String, Any]])
      }
    })

    // look in default output
    if (result.isEmpty && data.containsKey(defaultOutput)) {
      logger.info("Default Case")
      result = Some(defaultOutput, data.get(defaultOutput).asInstanceOf[java.util.Map[String, Any]])
    }

    // look in first (or any)
    if (result.isEmpty && !data.isEmpty) {
      logger.info("First Case")
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
    val yaml = new Yaml(new SafeConstructor(LoaderOptions()))
    yaml.load(yamlContent).asInstanceOf[java.util.Map[String, Any]]
  }

  private def getConfigFiles(projectRoot: String, extensions: Set[String], allowedFiles: Set[String] = Set()): List[String] = {
    def getListOfFiles(dir: String): List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }

    SourceFiles
      .determine(Set(projectRoot), extensions)
      .concat(
        getListOfFiles(projectRoot)
          .map(f => {
            f.getAbsolutePath
          })
      )
      .filter(_.matches(f".*(${allowedFiles.mkString("|")})"))
      .filter(file => Utilities.isFileProcessable(file, ruleCache) && (!file.matches(".*node_modules.*")))
      .distinct
  }

//
//  private def addSqlNodes(builder: DBTParserPass.this.DiffGraphBuilder, property: JavaProperty): Unit = {
//
//    val query        = property.value
//    val lineNumber   = property.lineNumber.getOrElse(-1).asInstanceOf[Int]
//    val columnNumber = property.columnNumber.getOrElse(-1).asInstanceOf[Int]
//    try {
//      SQLParser.parseSqlQuery(query) match {
//        case Some(parsedQueryList) =>
//          parsedQueryList.zipWithIndex.foreach { case (parsedQueryItem: SQLQuery, queryOrder) =>
//            buildAndReturnIndividualQueryNode(
//              builder,
//              property.sourceFileOut.next(),
//              parsedQueryItem,
//              query,
//              lineNumber,
//              queryOrder
//            )
//          }
//        case None =>
//          logger.debug("Failed to parse query: There might be a problem with the syntax.")
//          None
//      }
//    } catch {
//      case ex: Exception =>
//        logger.debug(s"Error while parsing SQL query at line $lineNumber: ${ex.getMessage}")
//        None
//    }
//  }
//
//  private def buildAndReturnIndividualQueryNode(
//    builder: DiffGraphBuilder,
//    fileNode: File,
//    queryModel: SQLQuery,
//    query: String,
//    queryLineNumber: Int,
//    queryOrder: Int
//  ): Unit = {
//    // Have added tableName in name key
//    // Have added columns in value key
//
//    val queryNode = NewSqlQueryNode().name(queryModel.queryType).code(query).lineNumber(queryLineNumber)
//
//    val tableNode = NewSqlTableNode()
//      .name(queryModel.table.name)
//      .code(query)
//      .lineNumber(queryLineNumber + queryModel.table.lineNumber - 1)
//      .columnNumber(queryModel.table.columnNumber)
//      .order(queryOrder)
//
//    builder.addEdge(queryNode, tableNode, EdgeTypes.AST)
//    builder.addEdge(queryNode, fileNode, EdgeTypes.SOURCE_FILE)
//    builder.addEdge(tableNode, fileNode, EdgeTypes.SOURCE_FILE)
//
//    queryModel.column.zipWithIndex.foreach { case (queryColumn: SQLColumn, columnIndex) =>
//      val columnNode = NewSqlColumnNode()
//        .name(queryColumn.name)
//        .code(queryColumn.name)
//        .lineNumber(queryLineNumber + queryColumn.lineNumber - 1)
//        .columnNumber(queryColumn.columnNumber)
//        .order(columnIndex)
//      builder.addEdge(tableNode, columnNode, EdgeTypes.AST)
//      builder.addEdge(columnNode, fileNode, EdgeTypes.SOURCE_FILE)
//
//    }
//  }
}
