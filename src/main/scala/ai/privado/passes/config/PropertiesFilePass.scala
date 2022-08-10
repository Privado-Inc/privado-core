package ai.privado.passes.config

import better.files.File
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewJavaProperty}
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.jdk.CollectionConverters._
import java.util.Properties
import scala.util.{Failure, Success, Try}
import io.shiftleft.semanticcpg.language._

/** This pass creates a graph layer for Java `.properties` files.
  */
class PropertiesFilePass(cpg: Cpg, projectRoot: String) extends SimpleCpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    propertiesFiles(projectRoot).foreach { file =>
      val fileNode      = addFileNode(file, builder)
      val propertyNodes = addPropertyNodesAndConnectToUsers(file, builder)
      propertyNodes.foreach { propertyNode =>
        builder.addEdge(propertyNode, fileNode, EdgeTypes.SOURCE_FILE)
      }
    }
  }

  private def addPropertyNodesAndConnectToUsers(
    file: String,
    builder: BatchedUpdate.DiffGraphBuilder
  ): List[NewJavaProperty] = {
    Try {
      obtainKeyValuePairs(file)
    } match {
      case Success(keyValuePairs) =>
        keyValuePairs.map { keyValuePairs =>
          val propertyNode = addPropertyNode(keyValuePairs, builder)
          connectToUsers(propertyNode, builder)
          propertyNode
        }
      case Failure(exception) =>
        logger.warn(exception.getMessage)
        List()
    }
  }

  private def connectToUsers(propertyNode: NewJavaProperty, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val literals = cpg.literal
      .codeExact("\"" + propertyNode.name + "\"")
      .where(_.inCall.name(".*getProperty"))
      .l
    literals.foreach { lit =>
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
    }
  }

  private def obtainKeyValuePairs(file: String): List[(String, String)] = {
    val properties  = new Properties()
    val inputStream = File(file).newFileInputStream
    properties.load(inputStream)
    inputStream.close()
    propertiesToKeyValuePairs(properties)
  }

  private def propertiesToKeyValuePairs(properties: Properties): List[(String, String)] = {
    properties
      .propertyNames()
      .asScala
      .collect { case key: String =>
        (key, properties.getProperty(key))
      }
      .toList
  }

  private def propertiesFiles(projectRoot: String): List[String] = {
    SourceFiles.determine(Set(projectRoot), Set(".properties"))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
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
