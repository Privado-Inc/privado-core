package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.javascript.processor.JavascriptProcessor
import ai.privado.model.Language
import ai.privado.utility.StatsRecorder

class TestCpgWithJavaScript(val fileSuffix: String, val language: Language.Value) extends TestCpg {
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
    new JavascriptProcessor(
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

class JavaScriptFrontendTestSuite(fileSuffix: String = ".js", language: Language.Value = Language.JAVASCRIPT)
    extends PrivadoBaseTestFixture(() => new TestCpgWithJavaScript(fileSuffix, language)) {}
