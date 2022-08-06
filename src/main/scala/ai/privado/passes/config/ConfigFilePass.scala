package ai.privado.passes.config

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate

class ConfigFilePass(cpg: Cpg, projectRoot: String) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    propertyFiles(projectRoot).foreach(println)
  }

  private def propertyFiles(projectRoot: String): List[String] = {
    io.joern.x2cpg.SourceFiles.determine(Set(projectRoot), Set(".properties"))
  }

}
