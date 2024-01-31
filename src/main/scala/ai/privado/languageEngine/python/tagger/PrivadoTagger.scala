package ai.privado.languageEngine.python.tagger

import ai.privado.cache.{DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.python.config.PythonDBConfigTagger
import ai.privado.languageEngine.python.feeder.StorageInheritRule
import ai.privado.languageEngine.python.passes.read.DatabaseReadPass
import ai.privado.languageEngine.python.tagger.collection.CollectionTagger
import ai.privado.languageEngine.python.tagger.sink.{InheritMethodTagger, PythonAPITagger}
import ai.privado.languageEngine.python.tagger.source.{IdentifierTagger, LiteralTagger}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.collection.WebFormsCollectionTagger
import ai.privado.tagger.sink.{LogShareSinkTagger, RegularSinkTagger}
import ai.privado.tagger.source.SqlQueryTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal
import ai.privado.utility.Utilities.ingressUrls

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache
  ): Traversal[Tag] = {

    logger.info("Starting tagging")

    new LiteralTagger(cpg, ruleCache).createAndApply()

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new PythonAPITagger(cpg, ruleCache, privadoInput = privadoInputConfig).createAndApply()

    new PythonDBConfigTagger(cpg).createAndApply()

    // Custom Rule tagging
    // Adding custom rule to cache
    StorageInheritRule.rules.foreach(ruleCache.setRuleInfo)

    new InheritMethodTagger(cpg, ruleCache).createAndApply()

    new RegularSinkTagger(cpg, ruleCache).createAndApply()

    new LogShareSinkTagger(cpg, ruleCache).createAndApply()

    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    ingressUrls = collectionTagger.getIngressUrls()

    new DatabaseReadPass(cpg, ruleCache, taggerCache, privadoInputConfig).createAndApply()

    new WebFormsCollectionTagger(cpg, ruleCache).createAndApply()

    logger.info("Done with tagging")
    cpg.tag
  }

}
