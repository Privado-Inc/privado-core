package ai.privado.languageEngine.kotlin.tagger

import ai.privado.cache.{AppCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.feeder.PermissionSourceRule
import ai.privado.languageEngine.java.feeder.StorageInheritRule
import ai.privado.languageEngine.java.tagger.collection.{CollectionTagger, MethodFullNameCollectionTagger}
import ai.privado.languageEngine.java.tagger.config.JavaDBConfigTagger
import ai.privado.languageEngine.java.tagger.sink.{InheritMethodTagger, JavaAPITagger}
import ai.privado.languageEngine.java.tagger.source.{IdentifierTagger, InSensitiveCallTagger}
import ai.privado.languageEngine.kotlin.feeder.StorageAnnotationRule
import ai.privado.languageEngine.kotlin.tagger.sink.StorageAnnotationTagger
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.collection.AndroidCollectionTagger
import ai.privado.tagger.sink.{APITagger, RegularSinkTagger}
import ai.privado.tagger.source.{AndroidXmlPermissionTagger, LiteralTagger, SqlQueryTagger}
import ai.privado.utility.Utilities.ingressUrls
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
    appCache: AppCache
  ): Traversal[Tag] = {

    logger.info("Starting tagging")

    new LiteralTagger(cpg, ruleCache).createAndApply()

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    new InSensitiveCallTagger(cpg, ruleCache, taggerCache).createAndApply()

    new AndroidXmlPermissionTagger(cpg, ruleCache, PermissionSourceRule.miniatureRuleList).createAndApply()

    new JavaDBConfigTagger(cpg).createAndApply()

    new RegularSinkTagger(cpg, ruleCache).createAndApply()

    // Custom Rule tagging
    if (!privadoInputConfig.ignoreInternalRules) {
      // Adding custom rule to cache
      StorageInheritRule.rules.foreach(ruleCache.setRuleInfo)
      new InheritMethodTagger(cpg, ruleCache).createAndApply()
      StorageAnnotationRule.rules.foreach(ruleCache.setRuleInfo)
      new StorageAnnotationTagger(cpg, ruleCache).createAndApply()
    }

    new APITagger(cpg, ruleCache, privadoInputConfig, appCache = appCache).createAndApply()

    new AndroidCollectionTagger(
      cpg,
      Paths.get(privadoInputConfig.sourceLocation.head).toAbsolutePath.toString,
      ruleCache
    ).createAndApply()

    // Tag by finding annotations that declare endpoints
    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    ingressUrls.addAll(collectionTagger.getIngressUrls())

    // Tag by finding methods that are declaring endpoints
    val methodFullNameTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
    methodFullNameTagger.createAndApply()
    ingressUrls.addAll(methodFullNameTagger.getIngressUrls())

    logger.info("Done with tagging")
    cpg.tag

  }
}
