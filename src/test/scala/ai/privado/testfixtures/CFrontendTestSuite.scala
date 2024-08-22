package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
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
    dependencies: List[DependencyInfo]
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
      databaseDetailsCache,
      propertyFilterCache
    )
  }
}

class CFrontendTestSuite(fileSuffix: String = ".cpp", language: Language.Value = Language.C)
    extends PrivadoBaseTestFixture(() => new TestCpgWithC(fileSuffix, language)) {}
