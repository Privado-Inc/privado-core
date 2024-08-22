package ai.privado.testfixtures

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.model.Language
import ai.privado.rule.RuleInfoTestData

trait LanguageFrontend {

  /** A standard file extension for the source code files of the given language. E.g. `.c` for C language
    */
  val fileSuffix: String
  val language: Language.Value

  private var privadoInput: Option[PrivadoInput]                     = None
  private var ruleCache: Option[RuleCache]                           = None
  private var auditCache: Option[AuditCache]                         = None
  private var dataFlowCache: Option[DataFlowCache]                   = None
  private var s3DatabaseDetailsCache: Option[S3DatabaseDetailsCache] = None
  private var appCache: Option[AppCache]                             = None
  private var propertyFilterCache: Option[PropertyFilterCache]       = None
  private var databaseDetailsCache: Option[DatabaseDetailsCache]     = None
  private var fileLinkingMetadata: Option[FileLinkingMetadata]       = None
  private var dependencies: Option[List[DependencyInfo]]             = None

  def setPrivadoInput(privadoInput: PrivadoInput): Unit = {
    if (this.privadoInput.isDefined) {
      throw new RuntimeException("PrivadoInput may only be set once per test")
    }
    this.privadoInput = Some(privadoInput)
  }

  def setRuleCache(ruleCache: RuleCache): Unit = {
    if (this.ruleCache.isDefined) {
      throw new RuntimeException("RuleCache may only be set once per test")
    }
    this.ruleCache = Some(ruleCache)
  }

  def setAuditCache(auditCache: AuditCache): Unit = {
    if (this.auditCache.isDefined) {
      throw new RuntimeException("AuditCache may only be set once per test")
    }
    this.auditCache = Some(auditCache)
  }

  def setDataFlowCache(dataFlowCache: DataFlowCache): Unit = {
    if (this.dataFlowCache.isDefined) {
      throw new RuntimeException("DataFlowCache may only be set once per test")
    }
    this.dataFlowCache = Some(dataFlowCache)
  }

  def setS3DatabaseDetailsCache(s3DatabaseDetailsCache: S3DatabaseDetailsCache): Unit = {
    if (this.s3DatabaseDetailsCache.isDefined) {
      throw new RuntimeException("S3DatabaseDetailsCache may only be set once per test")
    }
    this.s3DatabaseDetailsCache = Some(s3DatabaseDetailsCache)
  }

  def setAppCache(appCache: AppCache): Unit = {
    if (this.appCache.isDefined) {
      throw new RuntimeException("AppCache may only be set once per test")
    }
    this.appCache = Some(appCache)
  }

  def setPropertyFilterCache(propertyFilterCache: PropertyFilterCache): Unit = {
    if (this.propertyFilterCache.isDefined) {
      throw new RuntimeException("PropertyFilterCache may only be set once per test")
    }
    this.propertyFilterCache = Some(propertyFilterCache)
  }

  def setDatabaseDetailsCache(databaseDetailsCache: DatabaseDetailsCache): Unit = {
    if (this.databaseDetailsCache.isDefined) {
      throw new RuntimeException("DatabaseDetailsCache may only be set once per test")
    }
    this.databaseDetailsCache = Some(databaseDetailsCache)
  }

  def setFileLinkingMetadata(fileLinkingMetadata: FileLinkingMetadata): Unit = {
    if (this.fileLinkingMetadata.isDefined) {
      throw new RuntimeException("FileLinkingMetadata may only be set once per test")
    }
    this.fileLinkingMetadata = Some(fileLinkingMetadata)
  }

  def getFileLinkingMetadata: FileLinkingMetadata = this.fileLinkingMetadata.getOrElse(FileLinkingMetadata())

  def setDependencies(dependencies: List[DependencyInfo]): Unit = {
    if (this.dependencies.isDefined) {
      throw new RuntimeException("Dependencies may only be set once per test")
    }
    this.dependencies = Some(dependencies)
  }

  protected def getProcessor(sourceCodePath: java.io.File): BaseProcessor = {
    val privadoInput =
      this.privadoInput.getOrElse(PrivadoInput()).copy(sourceLocation = Set(sourceCodePath.getAbsolutePath))
    val appCache = this.appCache.getOrElse(AppCache())
    appCache.init(sourceCodePath.getAbsolutePath, privadoInput.excludeFileRegex)
    appCache.repoLanguage = language
    val auditCache = this.auditCache.getOrElse(AuditCache())
    getLanguageProcessor(
      this.ruleCache.getOrElse(RuleInfoTestData.ruleCache),
      privadoInput,
      this.dataFlowCache.getOrElse(DataFlowCache(privadoInput, auditCache)),
      auditCache,
      this.s3DatabaseDetailsCache.getOrElse(S3DatabaseDetailsCache()),
      appCache,
      this.propertyFilterCache.getOrElse(PropertyFilterCache()),
      this.databaseDetailsCache.getOrElse(DatabaseDetailsCache()),
      this.fileLinkingMetadata.getOrElse(FileLinkingMetadata()),
      this.dependencies.getOrElse(List())
    )
  }

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
  ): BaseProcessor
}
