package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import io.shiftleft.codepropertygraph.generated.Cpg
import io.circe._
import io.circe.syntax._
import io.joern.dataflowengineoss.language.Path

import java.util.Calendar
import scala.collection.mutable
import better.files.File
import org.slf4j.LoggerFactory

object JSONExporter {

  private val logger = LoggerFactory.getLogger(getClass)

  def fileExport(cpg: Cpg, outputFileName: String, repoPath: String, dataflows: Map[String, Path]) = {
    logger.info("Initiated exporter engine")
    val sourceExporter     = new SourceExporter(cpg)
    val dataflowExporter   = new DataflowExporter(cpg, dataflows)
    val collectionExporter = new CollectionExporter(cpg)
    val output             = mutable.LinkedHashMap[String, Json]()
    try {
      output.addOne(Constants.version       -> "1.0.0".asJson)
      output.addOne(Constants.createdAt     -> Calendar.getInstance().getTimeInMillis.asJson)
      output.addOne(Constants.gitMetadata   -> GitMetaDataExporter.getMetaData(repoPath).asJson)
      output.addOne(Constants.localScanPath -> repoPath.asJson)
      output.addOne(Constants.sources       -> sourceExporter.getSources.asJson)
      output.addOne(Constants.processing    -> sourceExporter.getProcessing.asJson)
      logger.info("Completed Source Exporting")

      val sinkSubCategories = RuleCache.getRule.sinks.map(sinkRule => sinkRule.catLevelTwo).toSet

      sinkSubCategories.foreach(sinkSubType => {
        output.addOne(sinkSubType -> dataflowExporter.getFlowByType(sinkSubType).asJson)
      })
      logger.info("Completed Sink Exporting")

      output.addOne("collections" -> collectionExporter.getCollections.asJson)
      logger.info("Completed Collections Exporting")

      File(repoPath + "/.privado").createDirectoryIfNotExists()
      val f = File(repoPath + "/.privado/" + outputFileName + ".json")
      f.write(output.asJson.toString())
      logger.info("Shutting down Exporter engine")

    } catch {
      case ex: Exception => println(ex.toString)
    }
  }

}
