package ai.privado.javascript.tagger.sink

import ai.privado.tagger.PrivadoSimplePass
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

class LeakageSinkTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val sinks = cpg.identifier(ruleInfo.patterns.head).astParent.isCall.name(".*(log|error|info|warn|error).*").l

    sinks.foreach(sink => addRuleTags(builder, sink, ruleInfo))

  }
}
