package ai.privado.testfixtures

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, PropertyFilterCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.php.processor.PhpProcessor
import ai.privado.model.Language
class TestCpgWithPhp(val fileSuffix: String, val language: Language.Value) extends TestCpg {
  protected def getLanguageProcessor(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache
  ): BaseProcessor = {
    new PhpProcessor(
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

class PhpFrontendTestSuite(fileSuffix: String = ".php", language: Language.Value = Language.PHP)
    extends PrivadoBaseTestFixture(() => new TestCpgWithPhp(fileSuffix, language)) {}
