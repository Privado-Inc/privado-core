package ai.privado.languageEngine.ruby.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.feeder.StorageInheritRule
import ai.privado.model.RuleInfo
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language.*

class InheritMethodTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  override def generateParts(): Array[RuleInfo] = StorageInheritRule.rules.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val callNode = cpg.call
      .and(
        _.filter(_.file.head.name.matches(ruleInfo.patterns.head)),
        _.filter(_.methodFullName.matches(ruleInfo.patterns(1)))
      )
      .l
    callNode.foreach(callNode => addRuleTags(builder, callNode, ruleInfo, ruleCache))
  }
}
