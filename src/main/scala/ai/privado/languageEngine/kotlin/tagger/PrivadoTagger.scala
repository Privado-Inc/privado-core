package ai.privado.languageEngine.kotlin.tagger

import ai.privado.cache.{AppCache, DataFlowCache, DatabaseDetailsCache, FileLinkingMetadata, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.feeder.PermissionSourceRule
import ai.privado.languageEngine.java.feeder.StorageInheritRule
import ai.privado.languageEngine.java.tagger.collection.{CollectionTagger, MethodFullNameCollectionTagger}
import ai.privado.languageEngine.java.tagger.config.JavaDBConfigTagger
import ai.privado.languageEngine.java.tagger.sink.api.{JavaAPISinkTagger, JavaAPITagger}
import ai.privado.languageEngine.java.tagger.sink.InheritMethodTagger
import ai.privado.languageEngine.java.tagger.sink.framework.flink.FlinkTagger
import ai.privado.languageEngine.java.tagger.source.*
import ai.privado.languageEngine.kotlin.feeder.StorageAnnotationRule
import ai.privado.languageEngine.kotlin.tagger.sink.StorageAnnotationTagger
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.collection.AndroidCollectionTagger
import ai.privado.tagger.sink.{APITagger, RegularSinkTagger}
import ai.privado.tagger.source.{AndroidXmlPermissionTagger, DEDTagger, LiteralTagger, SqlQueryTagger}
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.nio.file.Paths

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataflowCache: DataFlowCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder,
    fileLinkingMetadata: FileLinkingMetadata
  ): Traversal[Tag] = {

    logger.info("Starting tagging")

    new DEDTagger(cpg, ruleCache).createAndApply()

    new LiteralTagger(cpg, ruleCache).createAndApply()

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    SourceTagger.runTagger(cpg, ruleCache, taggerCache)

    new InSensitiveCallTagger(cpg, ruleCache, taggerCache).createAndApply()

    new AndroidXmlPermissionTagger(cpg, ruleCache, PermissionSourceRule.miniatureRuleList).createAndApply()

    new JavaDBConfigTagger(cpg, databaseDetailsCache).createAndApply()

    new RegularSinkTagger(cpg, ruleCache, databaseDetailsCache).createAndApply()

    // Custom Rule tagging
    if (!privadoInputConfig.ignoreInternalRules) {
      // Adding custom rule to cache
      StorageInheritRule.rules.foreach(ruleCache.setRuleInfo)
      new InheritMethodTagger(cpg, ruleCache).createAndApply()
      StorageAnnotationRule.rules.foreach(ruleCache.setRuleInfo)
      new StorageAnnotationTagger(cpg, ruleCache).createAndApply()
    }

    JavaAPISinkTagger.applyTagger(cpg, ruleCache, privadoInputConfig, appCache, statsRecorder, fileLinkingMetadata)

    FlinkTagger.applyTagger(cpg, ruleCache, privadoInputConfig, appCache, statsRecorder, fileLinkingMetadata)

    new AndroidCollectionTagger(
      cpg,
      Paths.get(privadoInputConfig.sourceLocation.head).toAbsolutePath.toString,
      ruleCache
    ).createAndApply()

    // Tag by finding annotations that declare endpoints
    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    appCache.ingressUrls.addAll(collectionTagger.getIngressUrls())

    // Tag by finding methods that are declaring endpoints
    val methodFullNameTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
    methodFullNameTagger.createAndApply()
    appCache.ingressUrls.addAll(methodFullNameTagger.getIngressUrls())

    logger.info("Done with tagging")
    cpg.tag

  }
}
