package ai.privado.exporter

import ai.privado.cache.{AppCache, EnvironmentConstant, RuleCache}
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants
import io.shiftleft.codepropertygraph.generated.Cpg
import io.circe._
import io.circe.syntax._
import io.joern.dataflowengineoss.language.Path
import org.apache.commons.io.FileUtils

import java.util.Calendar
import scala.collection.mutable
import better.files.File
import org.slf4j.LoggerFactory

import java.math.BigInteger

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
      output.addOne(Constants.coreVersion   -> EnvironmentConstant.privadoVersionCore.getOrElse("Not Detected").asJson)
      output.addOne(Constants.cliVersion    -> EnvironmentConstant.privadoVersionCli.getOrElse("Not Detected").asJson)
      output.addOne(Constants.mainVersion   -> AppCache.privadoVersionMain.asJson)
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
        dataflowsOutput.addOne(sinkSubType                -> dataflowExporter.getFlowByType(sinkSubType).asJson)
        MetricHandler.flowCategoryData.addOne(sinkSubType -> dataflowExporter.getFlowByType(sinkSubType).size)
      })

      output.addOne(Constants.dataFlow -> dataflowsOutput.asJson)
      logger.info("Completed Sink Exporting")

      output.addOne(Constants.collections -> collectionExporter.getCollections.asJson)
      logger.info("Completed Collections Exporting")
      output.addOne("policyViolations" -> policyExporter.getViolations.asJson)

      logger.info("Completed exporting policy violations")
      File(repoPath + "/.privado").createDirectoryIfNotExists()
      val f = File(repoPath + "/.privado/" + outputFileName + ".json")
      f.write(output.asJson.toString())
      logger.info("Shutting down Exporter engine")
      logger.info("Scanning Completed...")

      MetricHandler.metricsData("repoSize (in KB)") = Json.fromBigInt(
        FileUtils.sizeOfDirectoryAsBigInteger(new java.io.File(repoPath)).divide(BigInteger.valueOf(1024))
      )
      MetricHandler.metricsData("policyViolations") = Json.fromInt(policyExporter.getViolations.size)
      MetricHandler.metricsData("fileSize (in KB)") = Json.fromLong(f.size / 1024)

    } catch {
      case ex: Exception => println(ex.toString)
    }
  }

}
