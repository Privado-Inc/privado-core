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

object JSONExporter {

  def fileExport(cpg: Cpg, outputFileName: String, repoPath: String, dataflows: Map[String, Path]) = {
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

      val sinkSubCategories = RuleCache.getRule.sinks.map(sinkRule => sinkRule.catLevelTwo).toSet

      sinkSubCategories.foreach(sinkSubType => {
        output.addOne(sinkSubType -> dataflowExporter.getFlowByType(sinkSubType).asJson)
      })

      output.addOne("collections" -> collectionExporter.getCollections.asJson)

      File(repoPath + "/.privado").createDirectoryIfNotExists()
      val f = File(repoPath + "/.privado/" + outputFileName + ".json")
      f.write(output.asJson.toString())

    } catch {
      case ex: Exception => println(ex.toString)
    }
  }

}
