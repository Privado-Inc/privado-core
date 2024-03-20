package ai.privado.exporter.monolith

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache, S3DatabaseDetailsCache, TaggerCache}
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
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

object MonolithExporter {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /** Check if monolith flag is enabled, if yes export monolith results
    * @param cpg
    * @param repoItemTagName
    * @param outputFileName
    * @param sourceRepoLocation
    * @param dataflowMap
    * @param ruleCache
    * @param taggerCache
    * @param dataFlowCache
    * @param privadoInput
    * @return
    */
  def checkIfMonolithFlagEnabledAndExport(
    cpg: Cpg,
    outputFileName: String,
    sourceRepoLocation: String,
    dataflowMap: Map[String, Path],
    ruleCache: RuleCache,
    taggerCache: TaggerCache = new TaggerCache(),
    dataFlowCache: DataFlowCache,
    privadoInput: PrivadoInput,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache
  ): List[String] = {
    if (privadoInput.isMonolith) {
      // Export privado json for individual subProject/Repository Item
      cpg.tag
        .nameExact(Constants.monolithRepoItem)
        .value
        .dedup
        .l
        .par
        .flatMap(repoItemName =>
          MonolithExporter.fileExport(
            cpg,
            repoItemName,
            outputFileName,
            sourceRepoLocation,
            dataflowMap,
            ruleCache,
            taggerCache,
            dataFlowCache,
            privadoInput,
            s3DatabaseDetailsCache,
            appCache = appCache
          )
        )
        .l
    } else List()
  }

  def fileExport(
    cpg: Cpg,
    repoItemTagName: String,
    outputFileName: String,
    sourceRepoLocation: String,
    dataflows: Map[String, Path],
    ruleCache: RuleCache,
    taggerCache: TaggerCache = new TaggerCache(),
    dataFlowCache: DataFlowCache,
    privadoInput: PrivadoInput,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache
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
        sourceRepoLocation,
        dataflows,
        ruleCache,
        taggerCache,
        filterRepoItemDataflows(dataFlowCache, dataflows, repoItemTagName, privadoInput),
        privadoInput,
        s3DatabaseDetailsCache,
        repoItemTagName = Option(repoItemTagName),
        appCache = appCache
      )

      output.addOne(
        Constants.monolithRepoReachingFileList -> cpg.file
          .where(_.tag.nameExact(Constants.monolithRepoItem).valueExact(repoItemTagName))
          .tag
          .nameExact(Constants.monolithRepoReachingFileList)
          .value
          .headOption
          .getOrElse("")
          .split(",")
          .toList
          .asJson
      )

      val jsonFile = ExporterUtility.writeJsonToFile(
        cpg,
        outputFileName,
        sourceRepoLocation,
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
