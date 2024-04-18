package ai.privado.languageEngine.kotlin

import ai.privado.TestCpgBase
import ai.privado.cache.{PropertyFilterCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.kotlin.processor.KotlinProcessor
import ai.privado.model.Language
import ai.privado.rule.RuleInfoTestData
import ai.privado.utility.StatsRecorder

abstract class KotlinTestCpgBase(
  withRuleCache: RuleCache = RuleInfoTestData.ruleCache,
  withPrivadoInput: PrivadoInput = PrivadoInput()
) extends TestCpgBase(withPrivadoInput) {
  override def getProcessor(sourceCodeLocation: String): BaseProcessor = {
    appCache.init(sourceCodeLocation)
    appCache.repoLanguage = Language.KOTLIN
    new KotlinProcessor(
      withRuleCache,
      withPrivadoInput.copy(sourceLocation = Set(sourceCodeLocation)),
      sourceCodeLocation,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg = false,
      new PropertyFilterCache(),
      StatsRecorder()
    )
  }

}
