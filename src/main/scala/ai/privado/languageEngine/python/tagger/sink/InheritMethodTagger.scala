package ai.privado.languageEngine.python.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.python.feeder.StorageInheritRule
import ai.privado.model.RuleInfo
import ai.privado.tagger.sink.CustomInheritTagger
import io.shiftleft.codepropertygraph.generated.Cpg

class InheritMethodTagger(cpg: Cpg, ruleCache: RuleCache) extends CustomInheritTagger(cpg: Cpg, ruleCache: RuleCache) {

  override def generateParts(): Array[RuleInfo] = StorageInheritRule.rules.toArray

}
