package ai.privado.languageEngine.c.passes

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class CTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override protected def calls: Iterator[Call] = cpg.call
    .nameNot("<operator>.*", "<operators>.*")
    .filter(c => calleeNames(c).nonEmpty && (c.callee.isEmpty || c.callee.exists(_.isExternal)))
}
