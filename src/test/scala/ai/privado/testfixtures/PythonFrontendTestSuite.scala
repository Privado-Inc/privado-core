package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.python.processor.PythonProcessor
import ai.privado.model.*
import ai.privado.utility.StatsRecorder

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
    dependencies: List[DependencyInfo]
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
    databaseDetailsCache,
    propertyFilterCache,
    dependencies = dependencies
  )

}
class PythonFrontendTestSuite(fileSuffix: String = ".py", language: Language.Value = Language.PYTHON)
    extends PrivadoBaseTestFixture(() => new TestCpgWithPython(fileSuffix, language)) {}
