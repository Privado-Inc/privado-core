package ai.privado.languageEngine.default.tagger

import ai.privado.cache.{AppCache, DataFlowCache, DatabaseDetailsCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.default.passes.HighTouchDataflow
import ai.privado.tagger.source.SqlQueryTagger
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder
  ): Traversal[Tag] = {

    logger.info("Starting tagger")

    new SqlQueryTagger(cpg, ruleCache).createAndApply()
    new HighTouchSinkTagger(cpg, ruleCache).createAndApply()

    // Run this after SqlQueryTagger and HighTouchSinkTagger are invoked, so that tags are available
    HighTouchDataflow.generateDataflowAndAddToDataflowCache(cpg, dataFlowCache)

    logger.info("Done with tagging")
    cpg.tag
  }

}
