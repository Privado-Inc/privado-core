package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.Tagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._

object FlinkTagger extends Tagger {

  override def applyTagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput, appCache: AppCache): Unit = {
    // Run flink only if detected
    if (cpg.imports.importedEntity("org.apache.flink.*").nonEmpty) {

      println(s"Detected presence of Apache Flink")

      new FlinkUserDefinedSinkTagger(cpg, ruleCache).createAndApply()
      new FlinkConnectorInitialisationToFlinkSinkTagger(cpg, ruleCache, privadoInput, appCache).createAndApply()
    }
  }

}
