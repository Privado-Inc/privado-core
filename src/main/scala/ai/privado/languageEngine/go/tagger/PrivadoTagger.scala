package ai.privado.languageEngine.go.tagger

import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.tagger.PrivadoBaseTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.tagger.source.{LiteralTagger, SqlQueryTagger}
import ai.privado.cache.{RuleCache, TaggerCache}
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput
  ): Traversal[Tag] = {

    logger.info("Starting tagging")

    new LiteralTagger(cpg, ruleCache).createAndApply()

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    logger.info("Done with tagging")

    cpg.tag
  }

}
