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
import ai.privado.languageEngine.python.processor.PythonProcessor
import ai.privado.model.Language
import ai.privado.utility.StatsRecorder
import ai.privado.model.{CatLevelOne, Constants, FilterProperty, NodeType, RuleInfo}

class TestCpgWithPython(val fileSuffix: String, val language: Language.Value) extends TestCpg {

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
  ): BaseProcessor = new PythonProcessor(
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
class PythonFrontendTestSuite(fileSuffix: String = ".py", language: Language.Value = Language.PYTHON)
    extends PrivadoBaseTestFixture(() => new TestCpgWithPython(fileSuffix, language)) {}
