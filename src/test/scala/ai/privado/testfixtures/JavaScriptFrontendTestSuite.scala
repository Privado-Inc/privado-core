package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.javascript.processor.{JavascriptProcessor, JavascriptBaseCPGProcessor}
import ai.privado.model.*
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
    databaseDetailsCache: DatabaseDetailsCache,
    fileLinkingMetadata: FileLinkingMetadata,
    dependencies: List[DependencyInfo]
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
      databaseDetailsCache = databaseDetailsCache,
      propertyFilterCache = propertyFilterCache,
      fileLinkingMetadata = fileLinkingMetadata,
      dependencies = dependencies
    )
  }
}

class TestCpgWithJavaScriptBase(val fileSuffix: String, val language: Language.Value) extends TestCpg {
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
    dependencyInfo: List[DependencyInfo]
  ): BaseProcessor = {
    new JavascriptBaseCPGProcessor(
      ruleCache,
      privadoInput.copy(fileLinkingReport = true, isDeltaFileScan = true),
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

class JavaScriptFrontendTestSuite(fileSuffix: String = ".js", language: Language.Value = Language.JAVASCRIPT)
    extends PrivadoBaseTestFixture(() => new TestCpgWithJavaScript(fileSuffix, language)) {}

class JavaScriptBaseCpgFrontendTestSuite(fileSuffix: String = ".js", language: Language.Value = Language.JAVASCRIPT)
    extends PrivadoBaseTestFixture(() => new TestCpgWithJavaScriptBase(fileSuffix, language)) {}
