package ai.privado.languageEngine.go.tagger

import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.tagger.PrivadoBaseTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.tagger.source.{LiteralTagger, SqlQueryTagger}
import ai.privado.cache.{DataFlowCache, RuleCache, TaggerCache}
import ai.privado.languageEngine.go.tagger.collection.CollectionTagger
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.languageEngine.go.tagger.config.GoDBConfigTagger
import ai.privado.languageEngine.go.tagger.sink.GoAPITagger
import ai.privado.tagger.sink.RegularSinkTagger
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

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    new GoDBConfigTagger(cpg).createAndApply()

    new GoAPITagger(cpg, ruleCache, privadoInput = privadoInputConfig).createAndApply()

    new RegularSinkTagger(cpg, ruleCache).createAndApply()

    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    ingressUrls = collectionTagger.getIngressUrls()

    logger.info("Done with tagging")

    cpg.tag
  }

}
