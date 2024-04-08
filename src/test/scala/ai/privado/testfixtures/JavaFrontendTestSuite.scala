package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.java.processor.JavaProcessor

class TestCpgWithJava(val fileSuffix: String) extends TestCpg {
  protected def getLangaugeProcessor(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache
  ): BaseProcessor = {
    new JavaProcessor(
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

class JavaFrontendTestSuite(fileSuffix: String = ".java")
    extends PrivadoBaseTestFixture(() => new TestCpgWithJava(fileSuffix)) {}
