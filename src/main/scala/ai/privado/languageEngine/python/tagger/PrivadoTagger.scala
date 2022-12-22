package ai.privado.languageEngine.python.tagger

import ai.privado.model.{ConfigAndRules, NodeType}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.APITagger
import ai.privado.tagger.source.LiteralTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.util.Calendar

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(rules: ConfigAndRules): Traversal[Tag] = {

    logger.info("Starting tagging")

    logger.info("Starting tagging")
    val literalTagger = new LiteralTagger(cpg)
    val apiTagger = new APITagger(cpg)

    val sourceRules = rules.sources

    sourceRules.foreach(rule => {
      literalTagger.setRuleAndApply(rule)
    })

    println(s"${Calendar.getInstance().getTime} - APITagger invoked...")
    rules.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .foreach(rule => apiTagger.setRuleAndApply(rule))

    logger.info("Done with tagging")

    cpg.tag
  }

}
