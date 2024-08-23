package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.kotlin.processor.KotlinProcessor
import ai.privado.model.Language
import ai.privado.utility.StatsRecorder

class TestCpgWithKotlin(val fileSuffix: String, val language: Language.Value) extends TestCpg {
  protected def getLanguageProcessor(
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
    new KotlinProcessor(
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

class KotlinFrontendTestSuite(fileSuffix: String = ".kt", language: Language.Value = Language.KOTLIN)
    extends PrivadoBaseTestFixture(() => new TestCpgWithKotlin(fileSuffix, language)) {}
