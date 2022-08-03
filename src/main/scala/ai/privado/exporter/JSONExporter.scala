package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache}
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
    val policyExporter     = new PolicyExporter(dataflows)
    val output             = mutable.LinkedHashMap[String, Json]()
    try {
      output.addOne(Constants.version       -> "1.0.0".asJson)
      output.addOne(Constants.createdAt     -> Calendar.getInstance().getTimeInMillis.asJson)
      output.addOne(Constants.repoName      -> AppCache.repoName.asJson)
      output.addOne(Constants.gitMetadata   -> GitMetaDataExporter.getMetaData(repoPath).asJson)
      output.addOne(Constants.localScanPath -> AppCache.localScanPath.asJson)
      output.addOne(Constants.sources       -> sourceExporter.getSources.asJson)
      output.addOne(Constants.processing    -> sourceExporter.getProcessing.asJson)
      logger.info("Completed Source Exporting")

      val sinkSubCategories = RuleCache.getRule.sinks.map(sinkRule => sinkRule.catLevelTwo).toSet

      val dataflowsOutput = mutable.LinkedHashMap[String, Json]()
      sinkSubCategories.foreach(sinkSubType => {
        dataflowsOutput.addOne(sinkSubType -> dataflowExporter.getFlowByType(sinkSubType).asJson)
      })
      output.addOne(Constants.dataFlow -> dataflowsOutput.asJson)
      logger.info("Completed Sink Exporting")

      output.addOne("collections" -> collectionExporter.getCollections.asJson)
      logger.info("Completed Collections Exporting")

      output.addOne("policyViolations" -> policyExporter.getViolations.asJson)

      logger.info("Completed exporting policy violations")
      File(repoPath + "/.privado").createDirectoryIfNotExists()
      val f = File(repoPath + "/.privado/" + outputFileName + ".json")
      f.write(output.asJson.toString())
      logger.info("Shutting down Exporter engine")
      logger.info("Scanning Completed...")

    } catch {
      case ex: Exception => println(ex.toString)
    }
  }

}
