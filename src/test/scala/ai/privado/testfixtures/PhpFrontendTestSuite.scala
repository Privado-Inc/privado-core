package ai.privado.testfixtures

import ai.privado.cache.{
  AppCache,
  AuditCache,
  DataFlowCache,
  DatabaseDetailsCache,
  PropertyFilterCache,
  RuleCache,
  S3DatabaseDetailsCache
}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.php.processor.PhpProcessor
import ai.privado.model.*
import ai.privado.utility.StatsRecorder

class TestCpgWithPhp(val fileSuffix: String, val language: Language.Value) extends TestCpg {
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
    new PhpProcessor(
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

class PhpFrontendTestSuite(fileSuffix: String = ".php", language: Language.Value = Language.PHP)
    extends PrivadoBaseTestFixture(() => new TestCpgWithPhp(fileSuffix, language)) {
  val leakageRule = RuleInfo(
    "Log console",
    "Leakage",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List(".*log->info.*"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SINKS,
    catLevelTwo = Constants.leakages,
    Language.UNKNOWN,
    Array()
  )
}
