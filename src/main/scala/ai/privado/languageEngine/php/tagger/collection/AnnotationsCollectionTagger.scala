package ai.privado.languageEngine.php.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.collection.AnnotationsCollectionTaggerBase
import io.shiftleft.codepropertygraph.generated.Cpg

class AnnotationsCollectionTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg)
    with AnnotationsCollectionTaggerBase(cpg, ruleCache) {

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.annotations).toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    classUrlMap.addAll(collectAnnotatedUrlsFromClasses(ruleInfo.combinedRulePattern))
    val (methodUrls, collectionMethodsCache) = collectAnnotatedUrlsFromMethods(ruleInfo.combinedRulePattern)
    methodUrlMap.addAll(methodUrls)
    tagSources(builder, ruleInfo, collectionMethodsCache)
  }
}
