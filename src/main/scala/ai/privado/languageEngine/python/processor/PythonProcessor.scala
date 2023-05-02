package ai.privado.languageEngine.python.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.{AppCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.{ScanProcessor, TimeMetric}
import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import ai.privado.languageEngine.python.passes.config.PythonPropertyFilePass
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.python.semantic.Language._
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants}
import ai.privado.model.Constants.{
  cpgOutputFileName,
  outputAuditFileName,
  outputDirectoryName,
  outputFileName,
  outputIntermediateFileName
}
import ai.privado.semantic.Language._
import ai.privado.utility.UnresolvedReportUtility
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.config.ModuleFilePass
import ai.privado.languageEngine.java.passes.module.DependenciesNodePass
import io.joern.pysrc2cpg.{
  ImportsPass,
  InheritanceFullNamePass,
  Py2CpgOnFileSystem,
  Py2CpgOnFileSystemConfig,
  PythonNaiveCallLinker,
  PythonTypeHintCallLinker,
  PythonTypeRecoveryPass
}
import io.shiftleft.codepropertygraph
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import ai.privado.model.Language
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.javasrc2cpg.Config

import java.util.Calendar
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}
import ai.privado.passes.{HTMLParserPass, SQLParser}
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.shiftleft.codepropertygraph.generated.nodes.AstNode

import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer

object PythonProcessor {
  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    ruleCache: RuleCache,
    sourceRepoLocation: String
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
          new InheritanceFullNamePass(cpg).createAndApply()
          println(
            s"${TimeMetric.getNewTime()} - Run InheritanceFullNamePass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

//          println(s"${Calendar.getInstance().getTime} - HTML parser pass")
//          new HTMLParserPass(cpg, sourceRepoLocation, ruleCache).createAndApply()
//          println(
//            s"${TimeMetric.getNewTime()} - HTML parser pass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
//          )
          new PythonTypeRecoveryPass(cpg).createAndApply()
          println(
            s"${TimeMetric.getNewTime()} - Run PythonTypeRecovery done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )
          new PrivadoPythonTypeHintCallLinker(cpg).createAndApply()
          new PythonNaiveCallLinker(cpg).createAndApply()

          // Some of passes above create new methods, so, we
          // need to run the ASTLinkerPass one more time
          new AstLinkerPass(cpg).createAndApply()

          // Apply OSS Dataflow overlay
          new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))

          println(s"${Calendar.getInstance().getTime} - Processing property files pass")
          new PythonPropertyFilePass(cpg, sourceRepoLocation, ruleCache).createAndApply()
          println(
            s"${TimeMetric.getNewTime()} - Property file pass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          println(s"${Calendar.getInstance().getTime} - SQL parser pass")
          new SQLParser(cpg, sourceRepoLocation, ruleCache).createAndApply()
          println(
            s"${TimeMetric.getNewTime()} - SQL parser pass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          // Unresolved function report
          if (config.showUnresolvedFunctionsReport) {
            val path = s"${config.sourceLocation.head}/${Constants.outputDirectoryName}"
            UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.PYTHON)
          }

          // Run tagger
          println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
          val taggerCache = new TaggerCache
          cpg.runTagger(ruleCache, taggerCache)
          println(
            s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
          val dataflowMap = cpg.dataflow(ScanProcessor.config, ruleCache)
          println(s"\n${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
              .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${DataFlowCache.finalDataflow.size}")
          println(s"\n${TimeMetric.getNewTime()} - Code scanning is done in \t\t\t- ${TimeMetric.getTheTotalTime()}\n")
          println(s"${Calendar.getInstance().getTime} - Brewing result...")
          MetricHandler.setScanStatus(true)
          val errorMsg = new ListBuffer[String]()
          // Exporting Results
          JSONExporter.fileExport(cpg, outputFileName, sourceRepoLocation, dataflowMap, ruleCache, taggerCache) match {
            case Left(err) =>
              MetricHandler.otherErrorsOrWarnings.addOne(err)
              errorMsg += err
            case Right(_) =>
              println(s"Successfully exported output to '${AppCache.localScanPath}/$outputDirectoryName' folder")
              logger.debug(
                s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
              )
              val codelist = cpg.call
                .whereNot(_.methodFullName(Operators.ALL.asScala.toSeq: _*))
                .map(item => (item.methodFullName, item.location.filename))
                .dedup
                .l
              logger.debug(s"size of code : ${codelist.size}")
              codelist.foreach(item => logger.debug(item._1, item._2))
              logger.debug("Above we printed methodFullName")
              Right(())
          }

          // Exporting the Audit report
          if (ScanProcessor.config.generateAuditReport) {
            ExcelExporter.auditExport(
              outputAuditFileName,
              AuditReportEntryPoint.getAuditWorkbook(),
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
          }

          // Exporting the Intermediate report
          if (ScanProcessor.config.testOutput || ScanProcessor.config.generateAuditReport) {
            JSONExporter.IntermediateFileExport(
              outputIntermediateFileName,
              sourceRepoLocation,
              DataFlowCache.getIntermediateDataFlow()
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
          val cpgconfig =
            Config(outputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName")
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
  def createPythonCpg(ruleCache: RuleCache, sourceRepoLocation: String, lang: String): Either[String, Unit] = {

    println(s"${Calendar.getInstance().getTime} - Processing source code using $lang engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    // Converting path to absolute path, we may need that same as JS
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath
    val cpgOutputPath          = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    // TODO Discover ignoreVenvDir and set ignore true or flase based on user input
    val cpgconfig = Py2CpgOnFileSystemConfig(Paths.get(cpgOutputPath), absoluteSourceLocation, File(".venv").path, true)
    val xtocpg = new Py2CpgOnFileSystem().createCpg(cpgconfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      cpg
    }
    processCPG(xtocpg, ruleCache, sourceRepoLocation)
  }

}
