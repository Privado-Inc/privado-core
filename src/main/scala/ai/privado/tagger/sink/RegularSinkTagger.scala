package ai.privado.tagger.sink

import ai.privado.tagger.PrivadoSimplePass
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities._

class RegularSinkTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val sinks = cpg.call.methodFullName(ruleInfo.patterns.head)

    sinks.foreach(sink => addRuleTags(builder, sink, ruleInfo))

  }
}
