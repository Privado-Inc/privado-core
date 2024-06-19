package ai.privado.languageEngine.python.processor

import ai.privado.audit.{AuditReportEntryPoint, DEDSourceDiscovery}
import ai.privado.cache.*
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.python.config.PythonConfigPropertyPass
import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import ai.privado.languageEngine.python.passes.config.PythonPropertyLinkerPass
import ai.privado.languageEngine.python.semantic.Language.*
import ai.privado.languageEngine.python.tagger.PythonS3Tagger
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.{CatLevelOne, Constants, Language}
import ai.privado.passes.*
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass, StatsRecorder, UnresolvedReportUtility}
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.pysrc2cpg.*
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory

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
  statsRecorder: StatsRecorder,
  returnClosedCpg: Boolean = true,
  databaseDetailsCache: DatabaseDetailsCache = new DatabaseDetailsCache(),
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache()
) {
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
          statsRecorder.initiateNewStage("Default overlays")
          // Apply default overlays
          X2Cpg.applyDefaultOverlays(cpg)
          statsRecorder.endLastStage()
          statsRecorder.initiateNewStage("Overriden passes")
          new ImportsPass(cpg).createAndApply()
          new PythonImportResolverPass(cpg).createAndApply()
          new PythonInheritanceNamePass(cpg).createAndApply()
          new DynamicTypeHintFullNamePass(cpg).createAndApply()
          new PythonTypeRecoveryPassGenerator(cpg).generate().foreach(_.createAndApply())
          new PrivadoPythonTypeHintCallLinker(cpg).createAndApply()
          new NaiveCallLinker(cpg).createAndApply()

          // Some of passes above create new methods, so, we
          // need to run the ASTLinkerPass one more time
          new AstLinkerPass(cpg).createAndApply()
          statsRecorder.endLastStage()
          statsRecorder.initiateNewStage("Oss data flow")
          // Apply OSS Dataflow overlay
          new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
          if (privadoInput.enableLambdaFlows)
            new ExperimentalLambdaDataFlowSupportPass(cpg).createAndApply()
          statsRecorder.endLastStage()
          statsRecorder.setSupressSubstagesFlag(false)

          statsRecorder.initiateNewStage("Privado source passes")

          new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput)
            .createAndApply()

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
          statsRecorder.endLastStage()
          // Run tagger
          statsRecorder.initiateNewStage("Tagger ...")
          val taggerCache = new TaggerCache
          cpg.runTagger(
            ruleCache,
            taggerCache,
            privadoInputConfig = privadoInput,
            dataFlowCache,
            appCache,
            databaseDetailsCache,
            statsRecorder
          )

          statsRecorder.endLastStage()

          // we run S3 buckets detection after tagging
          new PythonS3Tagger(cpg, s3DatabaseDetailsCache, databaseDetailsCache).createAndApply()

          statsRecorder.endLastStage()

          statsRecorder.initiateNewStage("Finding data flows ...")
          val dataflowMap =
            Dataflow(cpg, statsRecorder).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache, appCache)
          statsRecorder.endLastStage()
          statsRecorder.justLogMessage(s"Processed final flows - ${dataFlowCache.getDataflowAfterDedup.size}")
          statsRecorder.initiateNewStage("Brewing result")
          MetricHandler.setScanStatus(true)
          val errorMsg = new ListBuffer[String]()

          // Exporting the DED Sources report
          if (privadoInput.dedSourceReport) {
            DEDSourceDiscovery.generateReport(Success(cpg), sourceRepoLocation, statsRecorder, Language.PYTHON) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                errorMsg += err
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported DED Source report to '${appCache.localScanPath}/$outputDirectoryName' folder..."
                )
            }
          }

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
                statsRecorder.justLogMessage(
                  s" Successfully exported Audit report to '${appCache.localScanPath}/$outputDirectoryName' folder..."
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
                statsRecorder.justLogMessage(
                  s" Successfully exported Unresolved flow output to '${appCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
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
                statsRecorder.justLogMessage(
                  s" Successfully exported intermediate output to '${appCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
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
  def createPythonCpg(): Either[String, Unit] = {
    statsRecorder.justLogMessage("Processing source code using Python engine")
    statsRecorder.initiateNewStage("Base source processing")

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
      statsRecorder.endLastStage()
      statsRecorder.justLogMessage(s"Total no of graph nodes -> ${cpg.graph.nodeCount()}")
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
