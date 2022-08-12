package ai.privado.tagger

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.ScanProcessor
import ai.privado.feeder.StorageInheritRule
import ai.privado.model.{ConfigAndRules, NodeType}
import ai.privado.tagger.collection.CollectionTagger
import ai.privado.tagger.sink.{APITagger, CustomInheritTagger, RegularSinkTagger}
import ai.privado.tagger.source.{IdentifierTagger, LiteralTagger}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def runTagger(rules: ConfigAndRules): Traversal[Tag] = {

    logger.info("Starting tagging")
    val literalTagger       = new LiteralTagger(cpg)
    val identifierTagger    = new IdentifierTagger(cpg)
    val apiTagger           = new APITagger(cpg)
    val regularSinkTagger   = new RegularSinkTagger(cpg)
    val customInheritTagger = new CustomInheritTagger(cpg)

    val sourceRules = rules.sources
    sourceRules.foreach(rule => {
      literalTagger.setRuleAndApply(rule)
      identifierTagger.setRuleAndApply(rule)
    })

    rules.sinks
      .filter(rule => rule.nodeType.equals(NodeType.REGULAR))
      .foreach(rule => regularSinkTagger.setRuleAndApply(rule))
    rules.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .foreach(rule => apiTagger.setRuleAndApply(rule))

    // Custom Rule tagging
    if (!ScanProcessor.config.ignoreInternalRules) {
      // Adding custom rule to cache
      StorageInheritRule.rules.foreach(RuleCache.setRuleInfo)
      StorageInheritRule.rules.foreach(rule => customInheritTagger.setRuleAndApply(rule))
    }

    val collectionTagger = new CollectionTagger(cpg, sourceRules)
    rules.collections.foreach(rule => collectionTagger.setRuleAndApply(rule))

    logger.info("Done with tagging")

    cpg.tag
  }

}
