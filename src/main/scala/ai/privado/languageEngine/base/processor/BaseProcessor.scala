package ai.privado.languageEngine.base.processor

import ai.privado.audit.{AuditReportEntryPoint, DEDSourceDiscovery, DependencyReport}
import ai.privado.cache.*
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.config.{JavaPropertyLinkerPass, ModuleFilePass}
import ai.privado.languageEngine.java.passes.module.{DependenciesCategoryPass, DependenciesNodePass}
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.Language.Language
import ai.privado.model.{CpgWithOutputMap, Language}
import ai.privado.passes.ExperimentalLambdaDataFlowSupportPass
import ai.privado.semantic.language.*
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.{PropertyParserPass, StatsRecorder, UnresolvedReportUtility}
import io.circe.Json
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.Config
import io.joern.x2cpg.X2CpgConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.{Logger, LoggerFactory}

import java.util.Calendar
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
abstract class BaseProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  lang: Language,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  statsRecorder: StatsRecorder,
  returnClosedCpg: Boolean,
  databaseDetailsCache: DatabaseDetailsCache,
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
  fileLinkingMetadata: FileLinkingMetadata = new FileLinkingMetadata()
) {

  val logger: Logger = LoggerFactory.getLogger(getClass)
  val errorMsg       = new ListBuffer[String]()

  /** Entry method to read files and generate output
    * @return
    */
  def processCpg(): Either[String, CpgWithOutputMap] = ???

  /** Takes care of consuming the Try[Cpg] and applying privado specific taggers and export json result
    * @param xtocpg
    * @return
    */

  def tagAndExport(xtocpg: Try[Cpg]): Either[String, CpgWithOutputMap] = {
    xtocpg match {
      case Success(cpg) =>
        try {
          statsRecorder.initiateNewStage("overriden overlay Processing")
          applyOverridenPasses(cpg)
          statsRecorder.endLastStage()
          statsRecorder.initiateNewStage("Privado source passes")
          applyPrivadoPasses(cpg).foreach(_.createAndApply())
          statsRecorder.endLastStage()

          statsRecorder.initiateNewStage("Run oss data flow")
          applyDataflowAndPostProcessingPasses(cpg)
          statsRecorder.endLastStage()
          statsRecorder.setSupressSubstagesFlag(false)
          applyTaggingAndExport(cpg) match
            case Left(err) =>
              logger.debug(s"Errors captured in scanning : $err")
              Left(err)
            case Right(cpgWithOutputMap) => Right(cpgWithOutputMap)
        } finally {
          if returnClosedCpg then cpg.close() // To not close cpg, and use it further, pass the returnClosedCpg as false
          import java.io.File
          val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
          val cpgFile       = new File(cpgOutputPath)
          statsRecorder.justLogMessage(
            s"Binary file size -- ${cpgFile.length()} in Bytes - ${cpgFile.length() * 0.000001} MB\n\n\n"
          )
        }
      case Failure(exception) =>
        logger.error("Error while parsing the source code!")
        logger.debug("Error : ", exception)
        MetricHandler.setScanStatus(false)
        Left("Error while parsing the source code: " + exception.toString)
    }
  }

  /** Method to apply privado specific passes
    * @param cpg
    * @return
    */
  def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = List()

  /** Method to apply Dataflow pass
    * @param cpg
    */

  def applyOverridenPasses(cpg: Cpg): Unit = List()

  def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    if (privadoInput.disableDataflowPass) logger.info("Skipping data flow overlay")
    else {
      logger.info("Applying data flow overlay")
      val context = new LayerCreatorContext(cpg)
      val options = new OssDataFlowOptions()
      new OssDataFlow(options).run(context)
    }
    if (privadoInput.enableLambdaFlows)
      new ExperimentalLambdaDataFlowSupportPass(cpg).createAndApply()
    logger.info("=====================")
  }

  /** Wrapper method which takes care of applying tagging and export
    * @param cpg
    * @return
    */
  def applyTaggingAndExport(cpg: Cpg): Either[String, CpgWithOutputMap] = {
    statsRecorder.initiateNewStage("Tagging ...")
    val taggerCache = new TaggerCache()
    runPrivadoTagger(cpg, taggerCache)
    statsRecorder.endLastStage()

    statsRecorder.initiateNewStage("Finding data flows ...")
    val dataflowMap =
      Dataflow(cpg, statsRecorder).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache, appCache)
    statsRecorder.endLastStage()

    if (privadoInput.fileLinkingReport) {
      // Add dataflow data to FileLinkingMetadata
      val dataflowFiles = dataflowMap.values.map(path => path.elements.flatMap(_.file.name).dedup.l).l
      fileLinkingMetadata.addToDataflowMap(dataflowFiles)
    }
    statsRecorder.justLogMessage(s"Processed final flows - ${dataFlowCache.getDataflowAfterDedup.size}")

    statsRecorder.initiateNewStage("Brewing result")
    val result = applyFinalExport(cpg, taggerCache, dataflowMap, s3DatabaseDetailsCache, appCache) match {
      case Left(err)        => Left(err)
      case Right(outputMap) => Right(CpgWithOutputMap(cpg, outputMap))
    }
    statsRecorder.endLastStage()
    result
  }

  def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = ???

  protected def applyFinalExport(
    cpg: Cpg,
    taggerCache: TaggerCache,
    dataflowMap: Map[String, Path],
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache
  ): Either[String, Map[String, Json]] = {

    val errorMsgs = ListBuffer[String]()
    reportUnresolvedMethods(cpg, lang)
    dedSourceReportExport(cpg, statsRecorder) match
      case Left(err) => errorMsgs.addOne(err)
      case Right(_)  =>

    auditReportExport(cpg, taggerCache) match
      case Left(err) => errorMsgs.addOne(err)
      case Right(_)  =>

    unresolvedReportExport(cpg) match
      case Left(err) => errorMsgs.addOne(err)
      case Right(_)  =>

    intermediateReportExport(cpg) match
      case Left(err) => errorMsgs.addOne(err)
      case Right(_)  =>

    fileLinkingReportExport(cpg) match
      case Left(err) => errorMsgs.addOne(err)
      case Right(_)  =>

    applyJsonExport(cpg, taggerCache, dataflowMap, s3DatabaseDetailsCache, appCache) match
      case Left(err) =>
        errorMsgs.addOne(err)
        Left(errorMsgs.mkString("\n"))
      case Right(outputMap) => Right(outputMap)

  }

  protected def applyJsonExport(
    cpg: Cpg,
    taggerCache: TaggerCache,
    dataflowMap: Map[String, Path],
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache
  ): Either[String, Map[String, Json]] = {
    MetricHandler.setScanStatus(true)
    // Exporting Results
    JSONExporter.fileExport(
      cpg,
      outputFileName,
      sourceRepoLocation,
      dataflowMap,
      ruleCache,
      taggerCache,
      dataFlowCache.getDataflowAfterDedup,
      privadoInput,
      List(),
      s3DatabaseDetailsCache,
      appCache,
      propertyFilterCache,
      databaseDetailsCache
    ) match {
      case Left(err) =>
        MetricHandler.otherErrorsOrWarnings.addOne(err)
        Left(err)
      case Right(outputJson) =>
        statsRecorder.justLogMessage(
          s"Successfully exported output to '${appCache.localScanPath}/$outputDirectoryName' folder..."
        )
        Right(outputJson)
    }
  }

  protected def auditReportExport(cpg: Cpg, taggerCache: TaggerCache): Either[String, Unit] = {
    // Exporting the Audit report
    if (privadoInput.generateAuditReport) {
      var dependencies = Set[ModuleDependency]()
      if (lang == Language.JAVA || lang == Language.KOTLIN) {
        val moduleCache: ModuleCache = new ModuleCache()
        new ModuleFilePass(cpg, sourceRepoLocation, moduleCache, ruleCache).createAndApply()
        new DependenciesNodePass(cpg, moduleCache).createAndApply()
        // Fetch all dependency after pass
        dependencies = DependencyReport.getDependencyList(Success(cpg))
        new DependenciesCategoryPass(cpg, ruleCache, dependencies.toList).createAndApply()
      }
      ExcelExporter.auditExport(
        outputAuditFileName,
        AuditReportEntryPoint
          .getAuditWorkbook(Success(cpg), taggerCache, dependencies, sourceRepoLocation, auditCache, ruleCache, lang),
        sourceRepoLocation
      ) match {
        case Left(err) =>
          MetricHandler.otherErrorsOrWarnings.addOne(err)
          Left(err)
        case Right(_) =>
          statsRecorder.justLogMessage(
            s"Successfully exported Audit report to '${appCache.localScanPath}/$outputDirectoryName' folder..."
          )
          Right(())
      }
    } else Right(())

  }

  protected def dedSourceReportExport(cpg: Cpg, statsRecorder: StatsRecorder): Either[String, Unit] = {
    // Exporting the DED Sources report
    if (privadoInput.dedSourceReport) {
      DEDSourceDiscovery.generateReport(Success(cpg), sourceRepoLocation, statsRecorder, lang) match {
        case Left(err) =>
          MetricHandler.otherErrorsOrWarnings.addOne(err)
          Left(err)
        case Right(_) =>
          statsRecorder.justLogMessage(
            s"Successfully exported DED Source report to '${appCache.localScanPath}/$outputDirectoryName' folder..."
          )
          Right(())
      }
    } else Right(())
  }

  protected def unresolvedReportExport(cpg: Cpg): Either[String, Unit] = {

    // Exporting the Unresolved report
    JSONExporter.UnresolvedFlowFileExport(
      outputUnresolvedFilename,
      sourceRepoLocation,
      dataFlowCache.getJsonFormatDataFlow(auditCache.unfilteredFlow)
    ) match {
      case Left(err) =>
        MetricHandler.otherErrorsOrWarnings.addOne(err)
        Left(err)
      case Right(_) =>
        statsRecorder.justLogMessage(
          s"Successfully exported Unresolved flow output to '${appCache.localScanPath}/$outputDirectoryName' folder..."
        )
        Right(())
    }
  }

  protected def intermediateReportExport(cpg: Cpg): Either[String, Unit] = {
    // Exporting the Intermediate report
    if (privadoInput.testOutput || privadoInput.generateAuditReport) {
      JSONExporter.IntermediateFileExport(
        outputIntermediateFileName,
        sourceRepoLocation,
        dataFlowCache.getJsonFormatDataFlow(dataFlowCache.getIntermediateDataFlow())
      ) match {
        case Left(err) =>
          MetricHandler.otherErrorsOrWarnings.addOne(err)
          Left(err)
        case Right(_) =>
          statsRecorder.justLogMessage(
            s"Successfully exported intermediate output to '${appCache.localScanPath}/$outputDirectoryName' folder..."
          )
          Right(())
      }
    } else
      Right(())
  }

  protected def fileLinkingReportExport(cpg: Cpg): Either[String, Unit] = {
    if (privadoInput.fileLinkingReport) {
      JSONExporter.fileLinkingExport(cpg, outputFileLinkingFileName, sourceRepoLocation, fileLinkingMetadata) match
        case Left(err) =>
          MetricHandler.otherErrorsOrWarnings.addOne(err)
          Left(err)
        case Right(_) =>
          statsRecorder.justLogMessage(
            s"Successfully exported file linking output to '${appCache.localScanPath}/$outputDirectoryName/$outputFileLinkingFileName' folder..."
          )
          Right(())
    } else
      Right(())
  }

  protected def reportUnresolvedMethods(cpg: Cpg, lang: Language): Unit = {
    // Unresolved function report
    if (privadoInput.showUnresolvedFunctionsReport) {
      try {
        val path = s"${privadoInput.sourceLocation.head}/$outputDirectoryName"
        UnresolvedReportUtility.reportUnresolvedMethods(Success(cpg), path, lang)
      } catch {
        case ex: Exception =>
          logger.debug(f"Failed to unresolved methods report: ${ex}")
      }
    }
  }

}
