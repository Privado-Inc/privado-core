package ai.privado.languageEngine.csharp

import ai.privado.TestCpgBase
import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.csharp.processor.CSharpProcessor
import ai.privado.model.Language
import ai.privado.rule.RuleInfoTestData

abstract class CSharpTestCpgBase(
  withRuleCache: RuleCache = RuleInfoTestData.ruleCache,
  withPrivadoInput: PrivadoInput = PrivadoInput()
) extends TestCpgBase(withPrivadoInput) {
  override def getProcessor(sourceCodeLocation: String): BaseProcessor = {
    appCache.init(sourceCodeLocation)
    appCache.repoLanguage = Language.CSHARP
    new CSharpProcessor(
      withRuleCache,
      withPrivadoInput.copy(sourceLocation = Set(sourceCodeLocation)),
      sourceCodeLocation,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg = false
    )
  }
}
