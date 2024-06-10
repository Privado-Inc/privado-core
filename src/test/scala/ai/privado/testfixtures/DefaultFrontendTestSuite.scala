package ai.privado.testfixtures

import ai.privado.cache.{
  AppCache,
  AuditCache,
  DataFlowCache,
  DatabaseDetailsCache,
  PropertyFilterCache,
  RuleCache,
  S3DatabaseDetailsCache
}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.default.processor.DefaultProcessor
import ai.privado.model.Language

class TestCpgWithDefaultLanguage(val fileSuffix: String, val language: Language.Value) extends TestCpg {
  protected def getLanguageProcessor(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache,
    databaseDetailsCache: DatabaseDetailsCache
  ): BaseProcessor = {
    new DefaultProcessor(
      ruleCache,
      privadoInput,
      privadoInput.sourceLocation.headOption.getOrElse(""),
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg = false,
      propertyFilterCache,
      databaseDetailsCache
    )
  }
}

class DefaultFrontendTestSuite(fileSuffix: String = ".java", language: Language.Value = Language.UNKNOWN)
    extends PrivadoBaseTestFixture(() => new TestCpgWithDefaultLanguage(fileSuffix, language)) {}
