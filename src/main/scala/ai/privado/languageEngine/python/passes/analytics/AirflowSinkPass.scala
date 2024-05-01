package ai.privado.languageEngine.python.passes.analytics

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.RuleInfo
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*


class AirflowSinkPass(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass{

  override def generateParts(): Array[RuleInfo] = {
    ruleCache.getRule.sinks.filter(rule => rule.name.contains("Operator")).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    cpg.call.name(ruleInfo.combinedRulePattern).foreach(node => {
      addRuleTags(builder, node, ruleInfo, ruleCache)
    })
  }
}
