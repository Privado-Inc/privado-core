package ai.privado.passes.config

import better.files.File
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import scala.jdk.CollectionConverters._

import java.util.Properties
import scala.util.{Failure, Success, Try}

/** This pass creates a graph layer for Java `.properties` files.
  */
class PropertiesFilePass(cpg: Cpg, projectRoot: String) extends SimpleCpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    propertiesFiles(projectRoot).foreach(addPropertiesFile(_, builder))
  }

  private def addPropertiesFile(file: String, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    addFileNode(file, builder)
    Try {
      parsePropertiesFile(file)
    } match {
      case Success(properties) =>
        val keyValuePairs = properties
          .propertyNames()
          .asScala
          .collect { case key: String =>
            (key, properties.getProperty(key))
          }
          .toList

        keyValuePairs.foreach(println)
      case Failure(exception) =>
        logger.warn(exception.getMessage)
    }
  }

  private def parsePropertiesFile(file: String): Properties = {
    val properties  = new Properties()
    val inputStream = File(file).newFileInputStream
    properties.load(inputStream)
    inputStream.close()
    properties
  }

  private def propertiesFiles(projectRoot: String): List[String] = {
    SourceFiles.determine(Set(projectRoot), Set(".properties"))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
  }

}
