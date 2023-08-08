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

import java.io.{File, StringReader}
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

import scala.jdk.CollectionConverters.*
import ai.privado.model.Language
import ai.privado.tagger.PrivadoParallelCpgPass
import org.yaml.snakeyaml.constructor.SafeConstructor

import scala.collection.mutable.ListBuffer


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

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    logger.info(f">>>>>> wooohoooo: file: $file")
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
      .filter(_.matches(f".*(${allowedFiles.mkString("|")}).*"))
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
