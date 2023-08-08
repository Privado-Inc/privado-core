package ai.privado.languageEngine.ruby.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.feeder.LeakageRule
import ai.privado.model.RuleInfo
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class LeakageTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  override def generateParts(): Array[RuleInfo] = LeakageRule.rules.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val leakages = cpg
      .call(ruleInfo.patterns(1))
      .filter(isFirstArgumentCall)
      .filter(_.argument.head.asInstanceOf[Call].name.matches(ruleInfo.patterns(0)))
      .l

    leakages.foreach(addRuleTags(builder, _, ruleInfo, ruleCache))

  }

  private def isFirstArgumentCall(callNode: Call) = callNode.argument.headOption.exists(_.isCall)

}
