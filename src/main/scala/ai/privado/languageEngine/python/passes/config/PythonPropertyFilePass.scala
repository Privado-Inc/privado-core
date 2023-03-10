package ai.privado.languageEngine.python.passes.config

import ai.privado.cache.RuleCache
import ai.privado.model.RuleInfo
import ai.privado.utility.Utilities
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, MethodParameterIn, NewFile, NewJavaProperty}
import io.shiftleft.passes.{ForkJoinParallelCpgPass, SimpleCpgPass}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal._

import scala.jdk.CollectionConverters._
import java.util.Properties
import scala.util.{Failure, Success, Try}
import io.shiftleft.semanticcpg.language._
import io.circe.yaml.parser
import com.github.wnameless.json.flattener.JsonFlattener
import io.shiftleft.codepropertygraph.generated.traversal.toJavaPropertyTraversalExtGen

import scala.io.Source

class PythonPropertyFilePass(cpg: Cpg, projectRoot: String) extends ForkJoinParallelCpgPass[String](cpg) {
  override def generateParts(): Array[String] = configFiles(projectRoot, Set(".ini", ".yml", ".yaml", ".env")).toArray

  private val logger = LoggerFactory.getLogger(getClass)

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {

    val fileNode      = addFileNode(file, builder);
    val propertyNodes = addPropertyNodesAndConnectToUsers(file, builder)
    propertyNodes.foreach(builder.addEdge(_, fileNode, EdgeTypes.SOURCE_FILE))
  }

  private def addPropertyNodesAndConnectToUsers(
    file: String,
    builder: BatchedUpdate.DiffGraphBuilder
  ): List[NewJavaProperty] = {
    Try {
      getDotenvKeyValuePairs(file)

    } match {
      case Success(keyValuePairs) =>
        val propertyNodes = keyValuePairs.map(addPropertyNode(_, builder))
        propertyNodes.foreach(propertyNode => {
          connectGetEnvironLiterals(propertyNode, builder)
        })

        propertyNodes
      case Failure(exception) =>
        logger.warn(exception.getMessage)
        List()
    }
  }

  private def getDotenvKeyValuePairs(file: String): List[(String, String)] = {
    val envProps = new Properties()
    Source
      .fromFile(file)
      .getLines
      .filter(line => line.trim.nonEmpty && !line.startsWith("#"))
      .foreach(line => {
        val Array(key, value) = line.split("=", 2)
        envProps.setProperty(key, value)
      })

    envProps.asScala
      .map(prop => (prop._1, prop._2))
      .toList
  }

  private def matchEnvironGetCalls(propertyName: String): List[Literal] = {
    cpg.literal
      .codeExact("\"" + propertyName + "\"")
      .where(_.inCall.methodFullName(".*environ.get"))
      .l
  }

  private def configFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    SourceFiles
      .determine(Set(projectRoot), extensions)
      .filter(Utilities.isFileProcessable)
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }

  private def connectGetEnvironLiterals(
    propertyNode: NewJavaProperty,
    builder: BatchedUpdate.DiffGraphBuilder
  ): Unit = {
    matchEnvironGetCalls(propertyNode.name.strip()).foreach(lit => {
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
      builder.addEdge(lit, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

  private def addPropertyNode(
    keyValuePair: (String, String),
    builder: BatchedUpdate.DiffGraphBuilder
  ): NewJavaProperty = {
    val (key, value) = keyValuePair
    val propertyNode = NewJavaProperty().name(key).value(value)
    builder.addNode(propertyNode)
    propertyNode
  }
}
