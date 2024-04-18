package ai.privado.testfixtures

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, PropertyFilterCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.kotlin.processor.KotlinProcessor
import ai.privado.model.Language

class TestCpgWithKotlin(val fileSuffix: String, val language: Language.Value) extends TestCpg {
  protected def getLanguageProcessor(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache
  ): BaseProcessor = {
    new KotlinProcessor(
      ruleCache,
      privadoInput,
      privadoInput.sourceLocation.head,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg = false,
      propertyFilterCache
    )
  }
}

class KotlinFrontendTestSuite(fileSuffix: String = ".kt", language: Language.Value = Language.KOTLIN)
    extends PrivadoBaseTestFixture(() => new TestCpgWithKotlin(fileSuffix, language)) {}
