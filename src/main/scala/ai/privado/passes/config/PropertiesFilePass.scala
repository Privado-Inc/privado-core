package ai.privado.passes.config

import better.files.File
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate

import java.util.Properties
import scala.util.{Failure, Success, Try}

/** This pass creates a graph layer for Java `.properties` files.
  */
class PropertiesFilePass(cpg: Cpg, projectRoot: String) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    propertiesFiles(projectRoot).foreach(addPropertiesFile(_, builder))
  }

  private def addPropertiesFile(file: String, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    addFileNode(file, builder)
    Try {
      parsePropertiesFile(file)
    } match {
      case Success(properties) =>
        println(properties)
      case Failure(exception) =>
        // TODO introduce logger
        println(exception.getMessage)
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
    io.joern.x2cpg.SourceFiles.determine(Set(projectRoot), Set(".properties"))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
  }

}
