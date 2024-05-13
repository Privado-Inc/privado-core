package ai.privado.languageEngine.go.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.*
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.go.passes.SQLQueryParser
import ai.privado.languageEngine.go.passes.config.GoYamlLinkerPass
import ai.privado.languageEngine.go.passes.orm.ORMParserPass
import ai.privado.languageEngine.go.semantic.Language.tagger
import ai.privado.languageEngine.java.language.*
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.{CatLevelOne, Constants, Language}
import ai.privado.passes.*
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass, StatsRecorder, UnresolvedReportUtility}
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory

import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class GoProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  returnClosedCpg: Boolean = true,
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
  statsRecorder: StatsRecorder
) {
  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    ruleCache: RuleCache,
    sourceRepoLocation: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) => {
        try {
          logger.info("Applying default overlays")
          logger.info("=====================")
          statsRecorder.initiateNewStage("Default overlays")
          // Apply default overlays
          X2Cpg.applyDefaultOverlays(cpg)
          statsRecorder.endLastStage()
          logger.info("Applying data flow overlay")
          statsRecorder.initiateNewStage("Oss data flow")
          val context = new LayerCreatorContext(cpg)
          val options = new OssDataFlowOptions()
          new OssDataFlow(options).run(context)
          if (ScanProcessor.config.enableLambdaFlows) {
            new ExperimentalLambdaDataFlowSupportPass(cpg).createAndApply()
          }
          statsRecorder.endLastStage()
          statsRecorder.setSupressSubstagesFlag(false)

          statsRecorder.initiateNewStage("Privado source passes")
          if (config.assetDiscovery)
            new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
              .createAndApply()
          else
            new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.GO, propertyFilterCache)
              .createAndApply()

          new GoYamlLinkerPass(cpg).createAndApply()
          new SQLParser(cpg, sourceRepoLocation, ruleCache).createAndApply()
          new SQLQueryParser(cpg).createAndApply()
          // Unresolved function report
          if (config.showUnresolvedFunctionsReport) {
            val path = s"${config.sourceLocation.head}/${Constants.outputDirectoryName}"
            UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.GO)
          }
          new ORMParserPass(cpg, ruleCache).createAndApply()
          statsRecorder.endLastStage()

          // Run tagger
          statsRecorder.initiateNewStage("Tagger ...")
          val taggerCache = new TaggerCache
          cpg.runTagger(
            ruleCache,
            taggerCache,
            privadoInputConfig = ScanProcessor.config.copy(),
            dataFlowCache,
            appCache
          )
          statsRecorder.endLastStage()

          statsRecorder.initiateNewStage("Finding data flows ...")
          val dataflowMap =
            Dataflow(cpg, statsRecorder).dataflow(ScanProcessor.config, ruleCache, dataFlowCache, auditCache, appCache)
          statsRecorder.endLastStage()
          statsRecorder.justLogMessage(s"Processed final flows - ${dataFlowCache.getDataflowAfterDedup.size}")
          statsRecorder.initiateNewStage("Brewing result")
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
            ScanProcessor.config,
            List(),
            s3DatabaseDetailsCache,
            appCache,
            propertyFilterCache
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
          if (ScanProcessor.config.generateAuditReport) {
            ExcelExporter.auditExport(
              outputAuditFileName,
              AuditReportEntryPoint.getAuditWorkbookPy(auditCache, xtocpg, ruleCache),
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
          if (ScanProcessor.config.testOutput || ScanProcessor.config.generateAuditReport) {
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
          statsRecorder.endLastStage()
          // Check if any of the export failed
          if (errorMsg.toList.isEmpty)
            Right(())
          else
            Left(errorMsg.toList.mkString("\n"))
        } finally {
          cpg.close()
          import java.io.File
          val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
          val cpgconfig = Config()
            .withInputPath(sourceRepoLocation)
            .withOutputPath(cpgOutputPath)
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

  /** Create cpg using Go Language
    *
    * @param sourceRepoLocation
    * @param lang
    * @return
    */
  def createGoCpg(): Either[String, Unit] = {
    statsRecorder.justLogMessage("Processing source code using GoLang engine")
    statsRecorder.initiateNewStage("Base source processing")

    // Converting path to absolute path, we may need that same as JS
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath
    val cpgOutputPath          = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    val excludeFileRegex       = ruleCache.getRule.exclusions.flatMap(rule => rule.patterns).mkString("|")

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    val cpgconfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)
      .withFetchDependencies(false)
    val xtocpg = new GoSrc2Cpg()
      .createCpg(cpgconfig)
      .map { cpg =>
        statsRecorder.endLastStage()
        cpg
      }
    processCPG(
      xtocpg,
      ruleCache,
      sourceRepoLocation,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      propertyFilterCache
    )
  }

}
