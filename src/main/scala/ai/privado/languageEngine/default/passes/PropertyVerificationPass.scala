package ai.privado.languageEngine.default.passes

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import overflowdb.traversal.ImplicitsTmp.toTraversalSugarExt
import ai.privado.languageEngine.java.language.StepsForProperty
import io.shiftleft.codepropertygraph.generated.traversal.{FileTraversalExtGen, toFileTraversalExtGen}

class PropertyVerificationPass(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {
  override def generateParts(): Array[_ <: AnyRef] = {
    // cpg.property.groupBy(prop => prop.sourceFileOut.name.headOption.getOrElse(""))
    cpg.property.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, part: JavaProperty): Unit = {
    println(
      s"Found ${part.name}: ${part.value} at line ${part.lineNumber} in file: ${part.sourceFileOut.name.headOption.getOrElse("No file found")}"
    )
  }
}
