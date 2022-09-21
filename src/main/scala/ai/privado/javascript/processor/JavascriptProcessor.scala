package ai.privado.javascript.processor

import ai.privado.cache.AppCache
import ai.privado.java.exporter.JSONExporter
import ai.privado.javascript.semantic.Language.tagger
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants}
import ai.privado.model.Constants.{outputDirectoryName, outputFileName}
import io.joern.dataflowengineoss.language.Path
import io.joern.joerncli.DefaultOverlays
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

import scala.util.{Failure, Success, Try}

object JavascriptProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    processedRules: ConfigAndRules,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpgWithoutDataflow) => {
        logger.info("Applying default overlays")
        cpgWithoutDataflow.close()
        val cpg = DefaultOverlays.create("cpg.bin")
        logger.info("=====================")

        // Run tagger
        println("Tagging source code with rules...")
        cpg.runTagger(processedRules)
        println("Finding source to sink flow of data...")
        val dataflowMap = Map[String, Path]()
        // cpg.dataflow

        println("Brewing result...")
        MetricHandler.setScanStatus(true)
        // Exporting
        JSONExporter.fileExport(cpg, outputFileName, sourceRepoLocation, dataflowMap) match {
          case Left(err) =>
            MetricHandler.otherErrorsOrWarnings.addOne(err)
            Left(err)
          case Right(_) =>
            println(s"Successfully exported output to '${AppCache.localScanPath}/$outputDirectoryName' folder")
            logger.debug(
              s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
            )
            Right(())
        }
      }

      case Failure(exception) => {
        logger.error("Error while parsing the source code!")
        logger.debug("Error : ", exception)
        MetricHandler.setScanStatus(false)
        Left("Error while parsing the source code: " + exception.toString)
      }
    }
  }

  /** Create cpg using Javascript Language
    *
    * @param sourceRepoLocation
    * @param lang
    * @return
    */
  def createJavaScriptCpg(
    processedRules: ConfigAndRules,
    sourceRepoLocation: String,
    lang: String
  ): Either[String, Unit] = {

    println(s"Processing source code using $lang engine")
    println("Parsing source code...")
    val cpgconfig =
      Config(inputPath = sourceRepoLocation)
    val xtocpg = new JsSrc2Cpg().createCpg(cpgconfig)
    processCPG(xtocpg, processedRules, sourceRepoLocation)
  }

}
