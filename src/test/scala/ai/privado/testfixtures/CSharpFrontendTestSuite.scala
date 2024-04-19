package ai.privado.testfixtures

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, PropertyFilterCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.csharp.processor.CSharpProcessor
import ai.privado.model.Language
import ai.privado.utility.StatsRecorder

class TestCpgWithCSharp(val fileSuffix: String, val language: Language.Value) extends TestCpg {
  protected def getLanguageProcessor(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache
  ): BaseProcessor = {
    new CSharpProcessor(
      ruleCache,
      privadoInput,
      privadoInput.sourceLocation.head,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg = false,
      propertyFilterCache,
      StatsRecorder()
    )
  }
}

class CSharpFrontendTestSuite(fileSuffix: String = ".cs", language: Language.Value = Language.CSHARP)
    extends PrivadoBaseTestFixture(() => new TestCpgWithCSharp(fileSuffix, language)) {}
