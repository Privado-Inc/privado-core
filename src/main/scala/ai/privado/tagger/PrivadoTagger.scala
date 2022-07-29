package ai.privado.tagger

import ai.privado.model.{CatLevelOne, NodeType, RuleInfo}
import ai.privado.tagger.collection.CollectionTagger
import ai.privado.tagger.sink.{APITagger, RegularSinkTagger}
import ai.privado.tagger.source.{IdentifierTagger, LiteralTagger}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) {
  val logger = LoggerFactory.getLogger(this.getClass)

  def runTagger(rules: List[RuleInfo]): Traversal[Tag] = {

    logger.info("Starting tagging")
    val literalTagger     = new LiteralTagger(cpg)
    val identifierTagger  = new IdentifierTagger(cpg)
    val apiTagger         = new APITagger(cpg)
    val regularSinkTagger = new RegularSinkTagger(cpg)

    val sourceRules = getRulesByCatLevelOne(rules, CatLevelOne.SOURCES)
    println(sourceRules)
    sourceRules.foreach(rule => {
      literalTagger.setRuleAndApply(rule)
      identifierTagger.setRuleAndApply(rule)
    })

    getRulesByCatLevelOne(rules, CatLevelOne.SINKS)
      .filter(rule => rule.nodeType.equals(NodeType.REGULAR))
      .foreach(rule => regularSinkTagger.setRuleAndApply(rule))
    getRulesByCatLevelOne(rules, CatLevelOne.SINKS)
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .foreach(rule => apiTagger.setRuleAndApply(rule))

    val collectionTagger = new CollectionTagger(cpg, sourceRules)
    getRulesByCatLevelOne(rules, CatLevelOne.COLLECTIONS).foreach(rule => collectionTagger.setRuleAndApply(rule))
    logger.info("Done with tagging")

    cpg.tag
  }

}
