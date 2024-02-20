package ai.privado.languageEngine.java.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

class MethodFullNameCollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends CollectionTagger(cpg, ruleCache) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  override def generateParts(): Array[RuleInfo] =
    // we want to look at methods, not annotations
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.default).toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val (methodUrls, collectionMethodsCache) = collectUrlsFromMethodCalls(ruleInfo.combinedRulePattern)
    methodUrlMap.addAll(methodUrls)
    // tag sources from Java collection tagger
    tagSources(builder, ruleInfo, collectionMethodsCache)
  }

  private def collectUrlsFromMethodCalls(combinedRulePatterns: String): (Map[Long, String], List[Method]) = {
    val methodEndpoints = cpg.call.methodFullName(combinedRulePatterns).l
    val methodMap       = methodEndpoints.method.l
    val literalMap = methodEndpoints.map(ma => ma.id() -> ma.argument.isLiteral.code.headOption.getOrElse("")).toMap
    (literalMap, methodMap)
  }

}
