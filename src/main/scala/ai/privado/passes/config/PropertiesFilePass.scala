package ai.privado.passes.config

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate

/** This pass creates a graph layer for Java `.properties` files.
  */
class PropertiesFilePass(cpg: Cpg, projectRoot: String) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    propertiesFiles(projectRoot).foreach(addFileNode(_, builder))
  }

  private def propertiesFiles(projectRoot: String): List[String] = {
    io.joern.x2cpg.SourceFiles.determine(Set(projectRoot), Set(".properties"))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
  }

}
