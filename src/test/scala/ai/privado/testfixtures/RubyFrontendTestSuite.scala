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
import ai.privado.languageEngine.ruby.processor.RubyProcessor
import ai.privado.model.Language
import ai.privado.utility.StatsRecorder

class TestCpgWithRuby(val fileSuffix: String, val language: Language.Value) extends TestCpg {

  override protected def getLanguageProcessor(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache,
    databaseDetailsCache: DatabaseDetailsCache
  ): BaseProcessor = {
    new RubyProcessor(
      ruleCache,
      privadoInput,
      privadoInput.sourceLocation.head,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      StatsRecorder(),
      returnClosedCpg = false,
      databaseDetailsCache,
      propertyFilterCache
    )
  }
}

class RubyFrontendTestSuite(fileSuffix: String = ".rb", language: Language.Value = Language.RUBY)
    extends PrivadoBaseTestFixture(() => new TestCpgWithRuby(fileSuffix, language)) {}
