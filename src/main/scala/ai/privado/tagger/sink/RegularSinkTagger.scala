package ai.privado.tagger.sink

import ai.privado.tagger.PrivadoSimplePass
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities._

class RegularSinkTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {
  lazy val cacheCall = cpg.call.or(_.nameNot("(<operator|<init).*")).l
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val sinks = cacheCall.methodFullName(ruleInfo.patterns.head).l

    sinks.foreach(sink => addRuleTags(builder, sink, ruleInfo))

  }
}
