package ai.privado.passes.config

import better.files.File
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, MethodParameterIn, NewFile, NewJavaProperty}
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import overflowdb.traversal._

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
          val paramsAndValues = cpg.annotation
            .fullName("org.springframework.*Value")
            .where(_.parameter)
            .where(_.parameterAssign.code("\\\"\\$\\{.*\\}\\\""))
            .map { x =>
              val literalName = x.parameterAssign.code.head
              val value       = literalName.slice(3, literalName.length - 2)
              (x.start.parameter.head, value)
            }
            .l
          connectToUsers(propertyNode, paramsAndValues, builder)
          propertyNode
        }
      case Failure(exception) =>
        logger.warn(exception.getMessage)
        List()
    }
  }

  /** In this method, we attempt to identify users of properties and connect them to property nodes.
    */
  private def connectToUsers(
    propertyNode: NewJavaProperty,
    paramsAndValues: List[(MethodParameterIn, String)],
    builder: BatchedUpdate.DiffGraphBuilder
  ): Unit = {
    matchingLiteralsInGetPropertyCalls(propertyNode.name).foreach { lit =>
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
    }

    paramsAndValues
      .filter { case (_, value) => propertyNode.name == value }
      .foreach { case (param, _) =>
        builder.addEdge(propertyNode, param, EdgeTypes.IS_USED_AT)
      }

  }

  private def matchingLiteralsInGetPropertyCalls(propertyName: String): List[Literal] = cpg.literal
    .codeExact("\"" + propertyName + "\"")
    .where(_.inCall.name(".*getProperty"))
    .l

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
