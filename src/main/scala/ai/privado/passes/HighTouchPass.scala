package ai.privado.passes

import ai.privado.cache.RuleCache
import ai.privado.tagger.PrivadoParallelCpgPass
import better.files.File
import better.files.File.VisitOptions
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg

class HighTouchPass(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {

  override def generateParts(): Array[String] = {
    SourceFiles
      .determine(projectRoot, Set(".yaml"), ignoredFilesRegex = Some(".*[.]privado.*".r))(VisitOptions.default)
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, part: String): Unit = {
    println()
  }
}
