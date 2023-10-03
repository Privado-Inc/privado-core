package ai.privado.languageEngine.kotlin.tagger

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.tagger.config.JavaDBConfigTagger
import ai.privado.languageEngine.java.tagger.sink.JavaAPITagger
import ai.privado.languageEngine.java.tagger.source.{IdentifierTagger, InSensitiveCallTagger}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.{APITagger, RegularSinkTagger}
import ai.privado.tagger.source.{LiteralTagger, SqlQueryTagger}
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

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    new InSensitiveCallTagger(cpg, ruleCache, taggerCache).createAndApply()

    new JavaDBConfigTagger(cpg).createAndApply()

    new RegularSinkTagger(cpg, ruleCache).createAndApply()

    new APITagger(cpg, ruleCache, privadoInputConfig).createAndApply()

    logger.info("Done with tagging")
    cpg.tag

  }
}
