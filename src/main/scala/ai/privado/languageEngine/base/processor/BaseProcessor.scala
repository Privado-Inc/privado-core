package ai.privado.languageEngine.base.processor

import ai.privado.audit.{AuditReportEntryPoint, DependencyReport}
import ai.privado.cache.*
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.java.passes.config.{JavaPropertyLinkerPass, ModuleFilePass}
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.module.{DependenciesCategoryPass, DependenciesNodePass}
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{
  cpgOutputFileName,
  outputAuditFileName,
  outputDirectoryName,
  outputFileName,
  outputIntermediateFileName,
  outputUnresolvedFilename
}
import ai.privado.model.{CpgWithOutputMap, Language}
import ai.privado.model.Language.Language
import ai.privado.passes.ExperimentalLambdaDataFlowSupportPass
import ai.privado.semantic.Language.*
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.{PropertyParserPass, UnresolvedReportUtility}
import io.circe.Json
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.Config
import io.joern.x2cpg.X2CpgConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.{Logger, LoggerFactory}

import java.util.Calendar
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
  returnClosedCpg: Boolean,
  databaseDetailsCache: DatabaseDetailsCache,
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache()
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
          applyPrivadoPasses(cpg).foreach(_.createAndApply())

          applyDataflowAndPostProcessingPasses(cpg)

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
          println(s"\n\n\nBinary file size -- ${cpgFile.length()} in Bytes - ${cpgFile.length() * 0.000001} MB\n\n\n")
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
  def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    logger.info("Applying data flow overlay")
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    if (privadoInput.enableLambdaFlows)
      new ExperimentalLambdaDataFlowSupportPass(cpg).createAndApply()
    logger.info("=====================")
    println(
      s"${TimeMetric.getNewTime()} - Run oss data flow is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
    )
  }

  /** Wrapper method which takes care of applying tagging and export
    * @param cpg
    * @return
    */
  def applyTaggingAndExport(cpg: Cpg): Either[String, CpgWithOutputMap] = {

    println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
    val taggerCache = new TaggerCache()
    runPrivadoTagger(cpg, taggerCache)
    println(
      s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
    val dataflowMap = cpg.dataflow(privadoInput, ruleCache, dataFlowCache, auditCache, appCache)
    println(s"${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
        .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${dataFlowCache.getDataflowAfterDedup.size}")
    println(s"\n\n${TimeMetric.getNewTime()} - Code scanning is done in \t\t\t- ${TimeMetric.getTheTotalTime()}\n\n")

    applyFinalExport(cpg, taggerCache, dataflowMap, s3DatabaseDetailsCache, appCache) match
      case Left(err)        => Left(err)
      case Right(outputMap) => Right(CpgWithOutputMap(cpg, outputMap))
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
    auditReportExport(cpg, taggerCache) match
      case Left(err) => errorMsgs.addOne(err)
      case Right(_)  =>

    unresolvedReportExport(cpg) match
      case Left(err) => errorMsgs.addOne(err)
      case Right(_)  =>

    intermediateReportExport(cpg) match
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
    println(s"${Calendar.getInstance().getTime} - Brewing result...")
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
        println(
          s"${Calendar.getInstance().getTime} - Successfully exported output to '${appCache.localScanPath}/$outputDirectoryName' folder..."
        )
        Right(outputJson)
    }
  }

  protected def auditReportExport(cpg: Cpg, taggerCache: TaggerCache): Either[String, Unit] = {
    // Exporting the Audit report
    if (privadoInput.generateAuditReport) {
      val moduleCache: ModuleCache = new ModuleCache()
      new ModuleFilePass(cpg, sourceRepoLocation, moduleCache, ruleCache).createAndApply()
      new DependenciesNodePass(cpg, moduleCache).createAndApply()
      // Fetch all dependency after pass
      val dependencies = DependencyReport.getDependencyList(Success(cpg))
      new DependenciesCategoryPass(cpg, ruleCache, dependencies.toList).createAndApply()
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
          println(
            s"${Calendar.getInstance().getTime} - Successfully exported Audit report to '${appCache.localScanPath}/$outputDirectoryName' folder..."
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
        println(
          s"${Calendar.getInstance().getTime} - Successfully exported Unresolved flow output to '${appCache.localScanPath}/$outputDirectoryName' folder..."
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
          println(
            s"${Calendar.getInstance().getTime} - Successfully exported intermediate output to '${appCache.localScanPath}/$outputDirectoryName' folder..."
          )
          Right(())
      }
    } else
      Right(())
  }

  protected def reportUnresolvedMethods(cpg: Cpg, lang: Language): Unit = {
    // Unresolved function report
    if (privadoInput.showUnresolvedFunctionsReport) {
      val path = s"${privadoInput.sourceLocation.head}/$outputDirectoryName"
      UnresolvedReportUtility.reportUnresolvedMethods(Success(cpg), path, lang)
    }
  }

}
