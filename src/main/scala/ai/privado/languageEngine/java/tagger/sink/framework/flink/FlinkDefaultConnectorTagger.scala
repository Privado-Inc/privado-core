package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class FlinkDefaultConnectorTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[Call](cpg)
    with TaggerHelper {

  override def generateParts(): Array[Call] = getFlinkDefaultConnectors(cpg, ruleCache).toArray

  override def runOnPart(builder: DiffGraphBuilder, sinkCallNode: Call): Unit = {

    val flinkSinks = sinkCallNode.method.ast.isCall.name(flinkSinkName).l
    if (flinkSinks.nonEmpty) {
      // Flink Sink and flink connector are present in the same method
      flinkSinks.foreach(copyTags(builder, ruleCache, List(sinkCallNode), _))
    }
    // Need to run dataflow to reach to the flink sink, tag local node which is the initialization for this sink
  }
}
