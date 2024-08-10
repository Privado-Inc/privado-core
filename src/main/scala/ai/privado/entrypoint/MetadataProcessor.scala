package ai.privado.entrypoint

import ai.privado.cache.{
  AppCache,
  AuditCache,
  DataFlowCache,
  DatabaseDetailsCache,
  FileLinkingMetadata,
  PropertyFilterCache,
  S3DatabaseDetailsCache
}
import ai.privado.languageEngine.javascript.processor.JavascriptBaseCPGProcessor
import ai.privado.languageEngine.python.processor.PythonBaseCPGProcessor
import ai.privado.metadata.SystemInfo
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants
import ai.privado.model.Language.UNKNOWN
import ai.privado.model.*
import better.files.File
import io.circe.Json
import io.joern.console.cpgcreation.guessLanguage
import ai.privado.entrypoint.MetadataProcessor.statsRecorder

import scala.util.{Failure, Success, Try}

object MetadataProcessor extends CommandProcessor with RuleProcessor {

  private val auditCache             = new AuditCache
  private val s3DatabaseDetailsCache = new S3DatabaseDetailsCache
  private val propertyFilterCache    = new PropertyFilterCache()
  private val databaseDetailsCache   = new DatabaseDetailsCache()
  private val fileLinkingMetadata    = new FileLinkingMetadata()

  def getDataflowCache: DataFlowCache = {
    new DataFlowCache(config, auditCache)
  }

  override def process(appCache: AppCache): Either[String, Unit] = {

    if (config.isDeltaFileScan) {
      processCpg(appCache)
    } else {
      Try(generateMetadata()) match
        case Failure(exception) =>
          println(s"Exception when processing metadata command : ${exception.toString}")
          Left(exception.toString)
        case Success(systemInfo) => Right(systemInfo)
    }
  }

  def generateMetadata(): SystemInfo = {
    val systemInfo = SystemInfo.getInfo
    SystemInfo.dumpInfoToFile(config.sourceLocation.head, Constants.systemInfoFileName, systemInfo)
    systemInfo
  }

  private def processCpg(appCache: AppCache): Either[String, Unit] = {
    val sourceRepoLocation = File(config.sourceLocation.head).path.toAbsolutePath.toString.stripSuffix("/")
    val excludeFileRegex   = config.excludeFileRegex
    // Setting up the application cache
    appCache.init(sourceRepoLocation, excludeFileRegex = excludeFileRegex)
    statsRecorder.initiateNewStage("Language detection")
    val languageDetected = if (config.forceLanguage == UNKNOWN) {
      val langDect = Try(guessLanguage(sourceRepoLocation))
      statsRecorder.endLastStage()
      Language.withJoernLangName(langDect)
    } else {
      statsRecorder.justLogMessage("Language forced ...")
      statsRecorder.endLastStage()
      config.forceLanguage
    }
    MetricHandler.metricsData("language") = Json.fromString(languageDetected.toString)

    languageDetected match {
      case Language.JAVASCRIPT =>
        statsRecorder.justLogMessage("Detected language 'JavaScript'")
        new JavascriptBaseCPGProcessor(
          getProcessedRule(Set(Language.JAVASCRIPT), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          AuditCache(),
          S3DatabaseDetailsCache(),
          appCache,
          statsRecorder = statsRecorder,
          databaseDetailsCache = databaseDetailsCache,
          propertyFilterCache = propertyFilterCache,
          fileLinkingMetadata = fileLinkingMetadata
        ).processCpg()
      case Language.PYTHON =>
        statsRecorder.justLogMessage("Detected language 'Python'")
        new PythonBaseCPGProcessor(
          getProcessedRule(Set(Language.PYTHON), appCache, statsRecorder, config),
          this.config,
          sourceRepoLocation,
          dataFlowCache = getDataflowCache,
          auditCache,
          s3DatabaseDetailsCache,
          appCache,
          propertyFilterCache = propertyFilterCache,
          databaseDetailsCache = databaseDetailsCache,
          statsRecorder = statsRecorder,
          fileLinkingMetadata = fileLinkingMetadata
        ).processCpg()
      case _ =>
        println("language not supported yet..")
    } match {
      case Left(err: String) => Left(err)
      case _ =>
        Right(
          ()
        ) // Ignore the result as not needed for further step, and due to discrepency in output for New and old frontends
    }
  }

}
