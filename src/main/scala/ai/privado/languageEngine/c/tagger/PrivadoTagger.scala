package ai.privado.languageEngine.c.tagger

import ai.privado.cache.{AppCache, DataFlowCache, DatabaseDetailsCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.c.tagger.source.IdentifierTagger
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.tagger.source.{DEDTagger, LiteralTagger, SqlQueryTagger}
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language.*

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    rules: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder
  ): Traversal[Tag] = {
    logger.info("Beginning tagging")

    new DEDTagger(cpg, rules).createAndApply()
    new LiteralTagger(cpg, rules).createAndApply()
    new IdentifierTagger(cpg, rules, taggerCache).createAndApply()
    new SqlQueryTagger(cpg, rules).createAndApply()

    new RegularSinkTagger(cpg, rules, databaseDetailsCache).createAndApply()

    logger.info("Finished tagging")
    cpg.tag
  }

}
