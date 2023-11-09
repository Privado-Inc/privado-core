package ai.privado.languageEngine.kotlin.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.kotlin.feeder.StorageAnnotationRule
import ai.privado.model.RuleInfo
import ai.privado.tagger.sink.CustomAnnotationTagger
import io.shiftleft.codepropertygraph.generated.Cpg

class StorageAnnotationTagger(cpg: Cpg, ruleCache: RuleCache) extends CustomAnnotationTagger(cpg, ruleCache) {

  override def generateParts(): Array[RuleInfo] = StorageAnnotationRule.rules.toArray

}
