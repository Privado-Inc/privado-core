package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
import ai.privado.utility.Utilities.addRuleTags

class FlinkUntaggedSinkTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[Call](cpg)
    with TaggerHelper {

  override def generateParts(): Array[Call] = cpg.call(flinkSinkName).whereNot(_.tag.nameExact(Constants.id)).toArray

  override def runOnPart(builder: DiffGraphBuilder, flinkSink: Call): Unit = {
    ruleCache.getRuleInfo(Constants.flinkCustomProducerRuleId) match
      case Some(flinkCustomRuleInfo) => addRuleTags(builder, flinkSink, flinkCustomRuleInfo, ruleCache)
      case None                      => println(s"Cannot tag remaining flink nodes as flink rule is missing")
  }
}
