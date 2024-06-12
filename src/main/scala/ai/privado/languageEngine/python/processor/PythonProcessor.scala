package ai.privado.languageEngine.python.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.*
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.python.config.PythonConfigPropertyPass
import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import ai.privado.languageEngine.python.passes.config.PythonPropertyLinkerPass
import ai.privado.languageEngine.python.semantic.Language.*
import ai.privado.languageEngine.python.tagger.PythonS3Tagger
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.{CatLevelOne, Constants, CpgWithOutputMap, Language}
import ai.privado.passes.*
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass, UnresolvedReportUtility}
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.pysrc2cpg.*
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}


class PythonProcessor(
                       ruleCache: RuleCache,
                       privadoInput: PrivadoInput,
                       sourceRepoLocation: String,
                       dataFlowCache: DataFlowCache,
                       auditCache: AuditCache,
                       s3DatabaseDetailsCache: S3DatabaseDetailsCache,
                       appCache: AppCache,
                       returnClosedCpg: Boolean = true,
                       propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
                       databaseDetailsCache: DatabaseDetailsCache = new DatabaseDetailsCache()
                     ) extends BaseProcessor(
  ruleCache,
  privadoInput,
  sourceRepoLocation,
  Language.PYTHON,
  dataFlowCache,
  auditCache,
  s3DatabaseDetailsCache,
  appCache,
  returnClosedCpg,
  databaseDetailsCache,
  propertyFilterCache
) {

  override val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    val passesList = List(new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput))

    passesList ++ List({
      if (privadoInput.assetDiscovery) {
        new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
        new PythonConfigPropertyPass(cpg)
      } else {
        new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.PYTHON, propertyFilterCache)
      }
    }) ++ List(
      new PythonPropertyLinkerPass(cpg),
      new SQLParser(cpg, sourceRepoLocation, ruleCache),
      new DBTParserPass(cpg, sourceRepoLocation, ruleCache, databaseDetailsCache)
    )
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache, appCache, databaseDetailsCache)
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {

  }
}

object PythonProcessor {
  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    privadoInput: PrivadoInput,
    ruleCache: RuleCache,
    sourceRepoLocation: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
    databaseDetailsCache: DatabaseDetailsCache = new DatabaseDetailsCache()
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) => {
        try {
          logger.info("Applying default overlays")
          logger.info("=====================")
          println(
            s"${TimeMetric.getNewTime()} - Run oss data flow is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          // Apply default overlays
          X2Cpg.applyDefaultOverlays(cpg)
          new ImportsPass(cpg).createAndApply()
          new PythonImportResolverPass(cpg).createAndApply()
          new PythonInheritanceNamePass(cpg).createAndApply()
          println(
            s"${TimeMetric.getNewTime()} - Run InheritanceFullNamePass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput)
            .createAndApply()

          new DynamicTypeHintFullNamePass(cpg).createAndApply()
          new PythonTypeRecoveryPassGenerator(cpg).generate().foreach(_.createAndApply())
          println(
            s"${TimeMetric.getNewTime()} - Run PythonTypeRecovery done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )
          new PrivadoPythonTypeHintCallLinker(cpg).createAndApply()
          new NaiveCallLinker(cpg).createAndApply()

          // Some of passes above create new methods, so, we
          // need to run the ASTLinkerPass one more time
          new AstLinkerPass(cpg).createAndApply()

          // Apply OSS Dataflow overlay
          new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
          if (privadoInput.enableLambdaFlows)
            new ExperimentalLambdaDataFlowSupportPass(cpg).createAndApply()

          if (privadoInput.assetDiscovery) {
            new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
              .createAndApply()
            new PythonConfigPropertyPass(cpg).createAndApply()
          } else
            new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.PYTHON, propertyFilterCache)
              .createAndApply()

          new PythonPropertyLinkerPass(cpg).createAndApply()

          new SQLParser(cpg, sourceRepoLocation, ruleCache).createAndApply()
          new SQLPropertyPass(cpg, sourceRepoLocation, ruleCache).createAndApply()
          new DBTParserPass(cpg, sourceRepoLocation, ruleCache, databaseDetailsCache).createAndApply()

          // Unresolved function report
          if (privadoInput.showUnresolvedFunctionsReport) {
            val path = s"${privadoInput.sourceLocation.head}/${Constants.outputDirectoryName}"
            UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.PYTHON)
          }

          // Run tagger
          println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
          val taggerCache = new TaggerCache
          cpg.runTagger(
            ruleCache,
            taggerCache,
            privadoInputConfig = privadoInput,
            dataFlowCache,
            appCache,
            databaseDetailsCache
          )
          println(
            s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          // we run S3 buckets detection after tagging
          new PythonS3Tagger(cpg, s3DatabaseDetailsCache, databaseDetailsCache).createAndApply()

          println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
          val dataflowMap = cpg.dataflow(privadoInput, ruleCache, dataFlowCache, auditCache, appCache)
          println(s"\n${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
              .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${dataFlowCache.getDataflowAfterDedup.size}")
          println(s"\n${TimeMetric.getNewTime()} - Code scanning is done in \t\t\t- ${TimeMetric.getTheTotalTime()}\n")
          println(s"${Calendar.getInstance().getTime} - Brewing result...")
          MetricHandler.setScanStatus(true)
          val errorMsg = new ListBuffer[String]()
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
              errorMsg += err
            case Right(_) =>
              println(s"Successfully exported output to '${appCache.localScanPath}/$outputDirectoryName' folder")
              logger.debug(
                s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
              )
              Right(())
          }

          // Exporting the Audit report
          if (privadoInput.generateAuditReport) {
            ExcelExporter.auditExport(
              outputAuditFileName,
              AuditReportEntryPoint
                .getAuditWorkbookForLanguage(
                  xtocpg,
                  taggerCache,
                  sourceRepoLocation,
                  auditCache,
                  ruleCache,
                  Language.PYTHON
                ),
              sourceRepoLocation
            ) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                errorMsg += err
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported Audit report to '${appCache.localScanPath}/$outputDirectoryName' folder..."
                )
            }

            // Exporting the Unresolved report
            JSONExporter.UnresolvedFlowFileExport(
              outputUnresolvedFilename,
              sourceRepoLocation,
              dataFlowCache.getJsonFormatDataFlow(auditCache.unfilteredFlow)
            ) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                errorMsg += err
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported Unresolved flow output to '${appCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
                )
            }
          }

          // Exporting the Intermediate report
          if (privadoInput.testOutput || privadoInput.generateAuditReport) {
            JSONExporter.IntermediateFileExport(
              outputIntermediateFileName,
              sourceRepoLocation,
              dataFlowCache.getJsonFormatDataFlow(dataFlowCache.getIntermediateDataFlow())
            ) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                errorMsg += err
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported intermediate output to '${appCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
                )
            }
          }

          // Check if any of the export failed
          if (errorMsg.toList.isEmpty)
            Right(())
          else
            Left(errorMsg.toList.mkString("\n"))
        } finally {
          cpg.close()
          import java.io.File
          val cpgconfig =
            Py2CpgOnFileSystemConfig().withOutputPath(s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName")
          val cpgFile = new File(cpgconfig.outputPath)
          println(s"\n\nBinary file size -- ${cpgFile.length()} in Bytes - ${cpgFile.length() * 0.000001} MB\n")
        }
      }

      case Failure(exception) =>
        logger.error("Error while parsing the source code!")
        logger.debug("Error : ", exception)
        MetricHandler.setScanStatus(false)
        Left("Error while parsing the source code: " + exception.toString)
    }
  }

  /** Create cpg using Python Language
    *
    * @param sourceRepoLocation
    * @param lang
    * @return
    */
  def createPythonCpg(
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    sourceRepoLocation: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache,
    databaseDetailsCache: DatabaseDetailsCache
  ): Either[String, Unit] = {

    println(s"${Calendar.getInstance().getTime} - Processing source code using Python engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    // Converting path to absolute path, we may need that same as JS
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath
    val cpgOutputPath          = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    val excludeFileRegex = ruleCache.getExclusionRegex
    // TODO Discover ignoreVenvDir and set ignore true or flase based on user input
    val cpgconfig = Py2CpgOnFileSystemConfig(Option(File(".venv").path), ignoreVenvDir = true)
      .withInputPath(absoluteSourceLocation.toString)
      .withOutputPath(Paths.get(cpgOutputPath).toString)
      .withIgnoredFilesRegex(excludeFileRegex)
    val xtocpg = new Py2CpgOnFileSystem().createCpg(cpgconfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      cpg
    }
    processCPG(
      xtocpg,
      privadoInput,
      ruleCache,
      sourceRepoLocation,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      propertyFilterCache,
      databaseDetailsCache
    )
  }

}
