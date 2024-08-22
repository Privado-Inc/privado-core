package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.java.processor.JavaProcessor
import ai.privado.model.Language
import ai.privado.utility.StatsRecorder

class TestCpgWithJava(val fileSuffix: String, val language: Language.Value) extends TestCpg {
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
    new JavaProcessor(
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
      propertyFilterCache,
      dependencies = dependencies
    )
  }
}

class JavaFrontendTestSuite(fileSuffix: String = ".java", language: Language.Value = Language.JAVA)
    extends PrivadoBaseTestFixture(() => new TestCpgWithJava(fileSuffix, language)) {}
