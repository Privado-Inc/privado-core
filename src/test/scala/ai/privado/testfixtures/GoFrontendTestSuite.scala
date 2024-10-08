package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.go.processor.GoProcessor
import ai.privado.model.*
import ai.privado.utility.StatsRecorder

class TestCpgWithGo(val fileSuffix: String, val language: Language.Value) extends TestCpg {

  override protected def getLanguageProcessor(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache,
    databaseDetailsCache: DatabaseDetailsCache,
    fileLinkingMetadata: FileLinkingMetadata,
    dependencies: List[DependencyInfo]
  ): BaseProcessor = {
    new GoProcessor(
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
      fileLinkingMetadata = fileLinkingMetadata,
      dependencies = dependencies
    )
  }
}

class GoFrontendTestSuite(fileSuffix: String = ".go", language: Language.Value = Language.GO)
    extends PrivadoBaseTestFixture(() => new TestCpgWithGo(fileSuffix, language)) {}
