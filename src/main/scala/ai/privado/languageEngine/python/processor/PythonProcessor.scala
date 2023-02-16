package ai.privado.languageEngine.python.processor

import ai.privado.cache.{AppCache, DataFlowCache}
import ai.privado.entrypoint.{ScanProcessor, TimeMetric}
import ai.privado.exporter.JSONExporter
import ai.privado.languageEngine.python.semantic.Language._
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants}
import ai.privado.model.Constants.{outputDirectoryName, outputFileName, outputIntermediateFileName}
import ai.privado.semantic.Language._
import ai.privado.utility.UnresolvedReportUtility
import ai.privado.entrypoint.ScanProcessor.config
import io.joern.pysrc2cpg.{
  ImportsPass,
  Py2CpgOnFileSystem,
  Py2CpgOnFileSystemConfig,
  PythonNaiveCallLinker,
  PythonTypeHintCallLinker,
  PythonTypeRecovery
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
import java.util.Calendar
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

object PythonProcessor {
  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    processedRules: ConfigAndRules,
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
          new PythonTypeRecovery(cpg).createAndApply()
          new PythonTypeHintCallLinker(cpg).createAndApply()
          new PythonNaiveCallLinker(cpg).createAndApply()

          // Apply OSS Dataflow overlay
          new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))

          // Run tagger
          println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
          cpg.runTagger(processedRules)
          println(
            s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
          val dataflowMap = cpg.dataflow
          println(s"\n${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
              .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${DataFlowCache.finalDataflow.size}")
          println(s"\n${TimeMetric.getNewTime()} - Code scanning is done in \t\t\t- ${TimeMetric.getTheTotalTime()}\n")
          println(s"${Calendar.getInstance().getTime} - Brewing result...")
          MetricHandler.setScanStatus(true)
          // Exporting Results
          if (ScanProcessor.config.testOutput) {
            JSONExporter.IntermediateFileExport(
              outputIntermediateFileName,
              sourceRepoLocation,
              DataFlowCache.getIntermediateDataFlow()
            ) match {
              case Left(err) =>
                MetricHandler.otherErrorsOrWarnings.addOne(err)
                Left(err)
              case Right(_) =>
                println(
                  s"${Calendar.getInstance().getTime} - Successfully exported intermediate output to '${AppCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
                )
                Right(())
            }
          }
          JSONExporter.fileExport(cpg, outputFileName, sourceRepoLocation, dataflowMap) match {
            case Left(err) =>
              MetricHandler.otherErrorsOrWarnings.addOne(err)
              Left(err)
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

        } finally {
          cpg.close()
          import java.io.File
          val cpgFile = new File(sourceRepoLocation)
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
    processedRules: ConfigAndRules,
    sourceRepoLocation: String,
    lang: String
  ): Either[String, Unit] = {

    println(s"${Calendar.getInstance().getTime} - Processing source code using $lang engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    // Converting path to absolute path, we may need that same as JS
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath
    val cpgOutFile             = File.newTemporaryFile(suffix = ".cpg.bin")
    cpgOutFile.deleteOnExit()
    // TODO Discover ignoreVenvDir and set ignore true or flase based on user input
    val cpgconfig = Py2CpgOnFileSystemConfig(cpgOutFile.path, absoluteSourceLocation, File(".venv").path, true)
    val xtocpg = new Py2CpgOnFileSystem().createCpg(cpgconfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      cpg
    }
    if (config.showUnresolvedFunctionsReport) {
      val path = s"${config.sourceLocation.head}/${Constants.outputDirectoryName}"
      UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.PYTHON)
    }
    processCPG(xtocpg, processedRules, sourceRepoLocation)
  }

}
