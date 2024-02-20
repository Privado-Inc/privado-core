package ai.privado.languageEngine.kotlin.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.CollectionTagger
import ai.privado.model.{Constants, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

class KotlinCollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends CollectionTagger(cpg, ruleCache) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.default).toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val (methodUrls, collectionMethodsCache) = collectUrlsFromMethodCalls(ruleInfo.combinedRulePattern)
    methodUrlMap.addAll(methodUrls)
    tagSources(builder, ruleInfo, collectionMethodsCache)
  }

  private def collectUrlsFromMethodCalls(combinedRulePatterns: String): (Map[Long, String], List[Method]) = {
    val methodEndpoints = cpg.call.methodFullName(combinedRulePatterns)
    (
      methodEndpoints.map(ma => ma.method.method.id() -> ma.argument.isLiteral.head.code).toMap,
      methodEndpoints.method.l
    )
  }

}
