package ai.privado.exporter.monolith

import ai.privado.cache.{AuditCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.ExporterUtility
import ai.privado.model.{Constants, DataFlowPathModel}
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
import io.shiftleft.semanticcpg.language.*

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
        filterRepoItemDataflows(dataFlowCache, dataflows, repoItemTagName, privadoInput),
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

  /** Only consider dataflows which have a source node tagged as a monolithRepoItem with the corresponding
    * repoItemTagName
    * @param dataflowModelList
    * @param dataflows
    * @param repoItemTagName
    */
  def filterRepoItemDataflows(
    dataFlowCache: DataFlowCache,
    dataflows: Map[String, Path],
    repoItemTagName: String,
    privadoInput: PrivadoInput
  ): List[DataFlowPathModel] = {

    // Keep generate audit false as well don't need audit report for individual repo Item
    val localDataflowCache =
      new DataFlowCache(privadoInput = privadoInput.copy(generateAuditReport = false), auditCache = new AuditCache)
    // Set up local dataflow cache
    localDataflowCache.dataflowsMapByType.putAll(dataFlowCache.dataflowsMapByType)

    dataFlowCache.getDataflowBeforeDedup
      .filter(model =>
        dataflows.get(model.pathId) match
          case Some(path) =>
            path.elements.headOption
              .exists(_.tag.nameExact(Constants.monolithRepoItem).valueExact(repoItemTagName).nonEmpty)
          case None => false
      )
      .foreach(dataflowModel => localDataflowCache.setDataflow(dataflowModel))

    localDataflowCache.getDataflowAfterDedup
  }

}
