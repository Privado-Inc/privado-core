package ai.privado.languageEngine.go.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.*
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.entrypoint.{ScanProcessor, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.go.passes.SQLQueryParser
import ai.privado.languageEngine.go.passes.orm.ORMParserPass
import ai.privado.languageEngine.go.semantic.Language.tagger
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.{CatLevelOne, Constants, Language}
import ai.privado.passes.*
import ai.privado.semantic.Language.*
import ai.privado.utility.UnresolvedReportUtility
import ai.privado.utility.Utilities.createCpgFolder
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

object GoProcessor {
  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    ruleCache: RuleCache,
    sourceRepoLocation: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) => {
        try {
          logger.info("Applying default overlays")
          logger.info("=====================")

          // Apply default overlays
          X2Cpg.applyDefaultOverlays(cpg)

          logger.info("Applying data flow overlay")
          val context = new LayerCreatorContext(cpg)
          val options = new OssDataFlowOptions()
          new OssDataFlow(options).run(context)
          if (ScanProcessor.config.enableLambdaFlows)
            new ExperimentalLambdaDataFlowSupportPass(cpg).createAndApply()
          logger.info("=====================")
          println(
            s"${TimeMetric.getNewTime()} - Run oss data flow is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          new SQLParser(cpg, sourceRepoLocation, ruleCache).createAndApply()
          new SQLQueryParser(cpg).createAndApply()
          // Unresolved function report
          if (config.showUnresolvedFunctionsReport) {
            val path = s"${config.sourceLocation.head}/${Constants.outputDirectoryName}"
            UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.GO)
          }
          new ORMParserPass(cpg, ruleCache).createAndApply()

          // Run tagger
          println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
          val taggerCache = new TaggerCache
          cpg.runTagger(ruleCache, taggerCache, privadoInputConfig = ScanProcessor.config.copy(), dataFlowCache)
          println(
            s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
          val dataflowMap = cpg.dataflow(ScanProcessor.config, ruleCache, dataFlowCache, auditCache)
          println(s"\n${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
              .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${dataFlowCache.finalDataflow.size}")
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
            dataFlowCache,
            ScanProcessor.config
          ) match {
            case Left(err) =>
              MetricHandler.otherErrorsOrWarnings.addOne(err)
              errorMsg += err
            case Right(_) =>
              println(s"Successfully exported output to '${AppCache.localScanPath}/$outputDirectoryName' folder")
              logger.debug(
                s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
              )
              Right(())
          }

          // Exporting the Audit report
          if (ScanProcessor.config.generateAuditReport) {
            ExcelExporter.auditExport(
              outputAuditFileName,
              AuditReportEntryPoint.getAuditWorkbookPy(auditCache),
              sourceRepoLocation
            ) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                errorMsg += err
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported Audit report to '${AppCache.localScanPath}/$outputDirectoryName' folder..."
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
                  s"${Calendar.getInstance().getTime} - Successfully exported Unresolved flow output to '${AppCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
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
                  s"${Calendar.getInstance().getTime} - Successfully exported intermediate output to '${AppCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
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
  def createGoCpg(
    ruleCache: RuleCache,
    sourceRepoLocation: String,
    lang: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache
  ): Either[String, Unit] = {

    println(s"${Calendar.getInstance().getTime} - Processing source code using $lang engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

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
      .withFetchDependencies(!config.skipDownloadDependencies)
    val xtocpg = new GoSrc2Cpg()
      .createCpg(cpgconfig)
      .map { cpg =>
        println(
          s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
        )
        cpg
      }
    processCPG(xtocpg, ruleCache, sourceRepoLocation, dataFlowCache, auditCache)
  }

}
