package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{Constants, FilterProperty, Language, RuleInfo}
import ai.privado.tagger.Tagger
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

object FlinkTagger extends Tagger with TaggerHelper {

  override def applyTagger(
    cpg: Cpg,
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    appCache: AppCache,
    statsRecorder: StatsRecorder
  ): Unit = {
    // Run flink only if detected
    if (cpg.imports.importedEntity("org.apache.flink.*").nonEmpty) {

      println(s"Detected presence of Apache Flink")

      generateAndAddCustomFlinkRuleToRuleCache(ruleCache)

      List(
        new FlinkUserDefinedConnectorTagger(cpg, ruleCache),
        new FlinkUserDefinedConnectorToFlinkSinkViaDataflowTagger(cpg, ruleCache, privadoInput, appCache),
        new FlinkDefaultConnectorTagger(cpg, ruleCache),
        new FlinkDefaultConnectorToFlinkSinkViaDataflowTagger(cpg, ruleCache, privadoInput, appCache),
        new FlinkUntaggedSinkTagger(cpg, ruleCache)
      ).foreach(_.createAndApply())
    }
  }

  /** Generates a custom Flink sink rule for being able to tag flink Sink Nodes
    * @param ruleCache
    */
  private def generateAndAddCustomFlinkRuleToRuleCache(ruleCache: RuleCache): Unit = {

    val flinkProducerRule = RuleInfo(
      Constants.flinkCustomProducerRuleId,
      "Apache Flink Producer",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array("flink.apache.org"),
      List(flinkSinkName),
      language = Language.JAVA
    )
    ruleCache.setRuleInfo(flinkProducerRule)
  }
}
