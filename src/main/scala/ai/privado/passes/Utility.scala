package ai.privado.passes

import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import overflowdb.BatchedUpdate

import java.nio.file.Path

trait Utility {

  def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder, projectRoot: String): NewFile = {
    val relativeFileName = Path.of(projectRoot).relativize(Path.of(name)).toString
    val fileNode         = NewFile().name(relativeFileName)
    builder.addNode(fileNode)
    fileNode
  }

  def combinedRulePattern(patterns: List[String]): String = {
    patterns.mkString("(", "|", ")")
  }

}
