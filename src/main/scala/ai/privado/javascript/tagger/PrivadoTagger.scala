package ai.privado.javascript.tagger

import ai.privado.javascript.tagger.sink.LeakageSinkTagger
import ai.privado.javascript.tagger.source.IdentifierTagger
import ai.privado.model.{ConfigAndRules, NodeType}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.source.LiteralTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(rules: ConfigAndRules): Traversal[Tag] = {

    logger.info("Starting tagging")
    val literalTagger    = new LiteralTagger(cpg)
    val identifierTagger = new IdentifierTagger(cpg)
    val leakageTagger    = new LeakageSinkTagger(cpg)

    val sourceRules = rules.sources
    sourceRules.foreach(rule => {
      literalTagger.setRuleAndApply(rule)
      identifierTagger.setRuleAndApply(rule)
    })

    rules.sinks
      .filter(rule => rule.nodeType.equals(NodeType.REGULAR))
      .foreach(rule => leakageTagger.setRuleAndApply(rule))

    logger.info("Done with tagging")

    cpg.tag
  }

}
