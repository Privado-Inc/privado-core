package ai.privado.exporter.monolith

import ai.privado.cache.{DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.ExporterUtility
import ai.privado.model.exporter.{
  CollectionModel,
  DataFlowSubCategoryModel,
  SinkModel,
  SourceModel,
  SourceProcessingModel
}
import io.circe.Json
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable

object MonolithExporter {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def fileExport(
    cpg: Cpg,
    repoItemTagName: String,
    outputFileName: String,
    repoPath: String,
    dataflows: Map[String, Path],
    ruleCache: RuleCache,
    taggerCache: TaggerCache = new TaggerCache(),
    dataFlowCache: DataFlowCache,
    privadoInput: PrivadoInput
  ): Option[String] = {

    try {
      val (
        output: mutable.LinkedHashMap[String, Json],
        sources: List[SourceModel],
        sinks: List[SinkModel],
        processing: List[SourceProcessingModel],
        dataflowsOutput: mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]],
        finalCollections: List[CollectionModel],
        complianceViolationSize: Int
      ) = ExporterUtility.generateIndividualComponent(
        cpg,
        outputFileName,
        repoPath,
        dataflows,
        ruleCache,
        taggerCache,
        dataFlowCache,
        privadoInput,
        repoItemTagName = Option(repoItemTagName)
      )

      val jsonFile = ExporterUtility.writeJsonToFile(
        cpg,
        outputFileName,
        repoPath,
        ruleCache,
        output.toMap,
        intermediateFolderName = Option(repoItemTagName.replaceAll("/", "-"))
      )
      Some(jsonFile.pathAsString)
    } catch {
      case ex: Exception =>
        println(s"Failed to export output for repository item : $repoItemTagName")
        logger.debug(s"Failed to export output for repository item : $repoItemTagName, $ex")
        None
    }
  }

}
