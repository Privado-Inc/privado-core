package ai.privado.languageEngine.default.tagger

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.tagger.source.SqlQueryTagger
import ai.privado.tagger.PrivadoBaseTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(ruleCache: RuleCache, taggerCache: TaggerCache): Traversal[Tag] = {

    logger.info("Starting tagger")

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    logger.info("Done with tagging")
    cpg.tag
  }

}
