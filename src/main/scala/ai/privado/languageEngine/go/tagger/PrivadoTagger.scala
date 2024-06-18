package ai.privado.languageEngine.go.tagger

import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.tagger.PrivadoBaseTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.tagger.source.{LiteralTagger, SqlQueryTagger}
import ai.privado.cache.{AppCache, DataFlowCache, DatabaseDetailsCache, RuleCache, TaggerCache}
import ai.privado.languageEngine.go.tagger.collection.CollectionTagger
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.languageEngine.go.tagger.config.GoDBConfigTagger
import ai.privado.languageEngine.go.tagger.sink.{GoAPISinkTagger, GoAPITagger}
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.utility.StatsRecorder
import ai.privado.utility.Utilities.{databaseURLPriority, ingressUrls}

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder
  ): Traversal[Tag] = {

    logger.info("Starting tagging")

    new LiteralTagger(cpg, ruleCache).createAndApply()

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    new GoDBConfigTagger(cpg, databaseDetailsCache).createAndApply()

    GoAPISinkTagger.applyTagger(cpg, ruleCache, privadoInputConfig, appCache, statsRecorder)

    new RegularSinkTagger(cpg, ruleCache, databaseDetailsCache).createAndApply()

    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    ingressUrls.addAll(collectionTagger.getIngressUrls())

    logger.info("Done with tagging")

    cpg.tag
  }

}
