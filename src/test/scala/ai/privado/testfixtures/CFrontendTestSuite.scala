package ai.privado.testfixtures

import ai.privado.cache.{
  AppCache,
  AuditCache,
  DataFlowCache,
  DatabaseDetailsCache,
  FileLinkingMetadata,
  PropertyFilterCache,
  RuleCache,
  S3DatabaseDetailsCache
}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.c.processor.CProcessor
import ai.privado.model.Language
import ai.privado.utility.StatsRecorder

class TestCpgWithC(val fileSuffix: String, val language: Language.Value) extends TestCpg {
  protected def getLanguageProcessor(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache,
    databaseDetailsCache: DatabaseDetailsCache,
    fileLinkingMetadata: FileLinkingMetadata
  ): BaseProcessor = {
    new CProcessor(
      ruleCache,
      privadoInput,
      privadoInput.sourceLocation.head,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      StatsRecorder(),
      returnClosedCpg = false,
      databaseDetailsCache = databaseDetailsCache,
      propertyFilterCache = propertyFilterCache,
      fileLinkingMetadata = fileLinkingMetadata
    )
  }
}

class CFrontendTestSuite(fileSuffix: String = ".cpp", language: Language.Value = Language.C)
    extends PrivadoBaseTestFixture(() => new TestCpgWithC(fileSuffix, language)) {}
