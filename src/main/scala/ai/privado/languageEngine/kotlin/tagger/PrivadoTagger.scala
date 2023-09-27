package ai.privado.languageEngine.kotlin.tagger

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.kotlin.tagger.source.IdentifierTagger
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.tagger.source.LiteralTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

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

    new RegularSinkTagger(cpg, ruleCache).createAndApply()

    logger.info("Done with tagging")
    cpg.tag

  }
}
