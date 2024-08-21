package ai.privado.languageEngine.python.tagger

import ai.privado.cache.{
  AppCache,
  DataFlowCache,
  DatabaseDetailsCache,
  FileLinkingMetadata,
  RuleCache,
  S3DatabaseDetailsCache,
  TaggerCache
}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.python.config.PythonDBConfigTagger
import ai.privado.languageEngine.python.feeder.StorageInheritRule
import ai.privado.languageEngine.python.passes.read.DatabaseReadPass
import ai.privado.languageEngine.python.tagger.collection.{CherryPyTagger, CollectionTagger}
import ai.privado.languageEngine.python.tagger.sink.{
  AirflowOperatorSinkTagger,
  InheritMethodTagger,
  PythonAPISinkTagger,
  PythonAPITagger
}
import ai.privado.languageEngine.python.tagger.source.{IdentifierTagger, LiteralTagger}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.collection.WebFormsCollectionTagger
import ai.privado.tagger.sink.{LogShareSinkTagger, RegularSinkTagger}
import ai.privado.tagger.source.{DEDTagger, SqlQueryTagger}
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder,
    fileLinkingMetadata: FileLinkingMetadata
  ): Iterator[Tag] = {

    logger.info("Starting tagging")
    new DEDTagger(cpg, ruleCache).createAndApply()

    LiteralTagger.tag(cpg, ruleCache)

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    PythonAPISinkTagger.applyTagger(cpg, ruleCache, privadoInputConfig, appCache, statsRecorder, fileLinkingMetadata)

    new PythonDBConfigTagger(cpg, databaseDetailsCache).createAndApply()

    // Custom Rule tagging
    // Adding custom rule to cache
    StorageInheritRule.rules.foreach(ruleCache.setRuleInfo)

    new InheritMethodTagger(cpg, ruleCache).createAndApply()

    new RegularSinkTagger(cpg, ruleCache, databaseDetailsCache).createAndApply()

    new LogShareSinkTagger(cpg, ruleCache).createAndApply()

    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    new CherryPyTagger(cpg, ruleCache).createAndApply()
    appCache.ingressUrls.addAll(collectionTagger.getIngressUrls())

    new DatabaseReadPass(cpg, ruleCache, taggerCache, privadoInputConfig, appCache).createAndApply()

    new WebFormsCollectionTagger(cpg, ruleCache).createAndApply()

    new AirflowOperatorSinkTagger(cpg, ruleCache).createAndApply()

    new PythonS3Tagger(cpg, s3DatabaseDetailsCache, databaseDetailsCache).createAndApply()

    logger.info("Done with tagging")
    cpg.tag
  }

}
