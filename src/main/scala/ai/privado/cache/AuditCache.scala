package ai.privado.cache

import ai.privado.dataflow.DuplicateFlowProcessor
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.DataFlowPathModel
import io.joern.dataflowengineoss.language.Path
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

object AuditCache {

  private val logger = LoggerFactory.getLogger(getClass)

  private val flowPathBeforeFirstFiltering: mutable.Set[SourcePathInfo] = new mutable.HashSet[SourcePathInfo]()

  private val flowPathBeforeSecondFiltering: mutable.Set[SourcePathInfo] = new mutable.HashSet[SourcePathInfo]()

  private val flowPathBeforeFirstDedup: mutable.Set[SourcePathInfo] = new mutable.HashSet[SourcePathInfo]()

  private val flowPathBeforeSecondDedup: mutable.Set[SourcePathInfo] = new mutable.HashSet[SourcePathInfo]()

  private val flowPathFinal: mutable.Set[SourcePathInfo] = new mutable.HashSet[SourcePathInfo]()

  private var dataflowMapByPathId: Map[String, Path] = null

  def getFlowBeforeFirstFiltering: Set[SourcePathInfo] = flowPathBeforeFirstFiltering.toSet

  def addIntoBeforeFirstFiltering(sourcePathInfo: SourcePathInfo): Unit = flowPathBeforeFirstFiltering += sourcePathInfo

  def addIntoBeforeSecondFiltering(sourcePathInfo: SourcePathInfo): Unit =
    flowPathBeforeSecondFiltering += sourcePathInfo

  def checkFlowExistInSecondFiltering(sourcePathInfo: SourcePathInfo): Boolean =
    flowPathBeforeSecondFiltering.contains(sourcePathInfo)

  def pathIdExist(pathId: String): Boolean =
    if (dataflowMapByPathId != null && dataflowMapByPathId.contains(pathId)) true else false

  def getPathFromId(pathId: String): Path = dataflowMapByPathId(pathId)

  def addIntoBeforeFirstFiltering(
    dataflowPathsUnfiltered: List[Path],
    privadoScanConfig: PrivadoInput,
    ruleCache: RuleCache
  ): Unit = {

    dataflowMapByPathId = getDataflowPathAndIdMap(dataflowPathsUnfiltered)

    val expendedSourceSinkInfo =
      DuplicateFlowProcessor.processExpendedSourceSinkData(dataflowMapByPathId, privadoScanConfig, ruleCache, false)

    expendedSourceSinkInfo.foreach(flowInfo => {
      addIntoBeforeFirstFiltering(SourcePathInfo(flowInfo.pathSourceId, flowInfo.sinkId, flowInfo.sinkPathId))
    })
  }

  private def getDataflowPathAndIdMap(dataflowPathsUnfiltered: List[Path]): Map[String, Path] = {

    dataflowMapByPathId = dataflowPathsUnfiltered
      .flatMap(dataflow => {
        DuplicateFlowProcessor.calculatePathId(dataflow) match {
          case Success(pathId) => Some(pathId, dataflow)
          case Failure(e) =>
            logger.debug("Exception : ", e)
            None
        }
      })
      .toMap

    dataflowMapByPathId
  }

  def addIntoBeforeFirstDedup(
    dataFlow: mutable.HashMap[String, mutable.HashMap[String, ListBuffer[DataFlowPathModel]]]
  ): Unit = {
    dataFlow.foreach(flow => {
      flow._2.foreach(fileInfo => {
        fileInfo._2.foreach(dataflowModel => {
          val newDataFlow = SourcePathInfo(dataflowModel.sourceId, dataflowModel.sinkId, dataflowModel.pathId)
          if (!flowPathBeforeFirstDedup.contains(newDataFlow)) {
            flowPathBeforeFirstDedup += newDataFlow
          }
        })
      })
    })
  }

  def checkFlowExistInFirstDedup(sourcePathInfo: SourcePathInfo): Boolean =
    flowPathBeforeFirstDedup.contains(sourcePathInfo)

  def addIntoBeforeSecondDedup(
    dataFlow: mutable.HashMap[String, mutable.HashMap[String, ListBuffer[DataFlowPathModel]]]
  ): Unit = {
    dataFlow.foreach(flow => {
      flow._2.foreach(fileInfo => {
        fileInfo._2.foreach(dataflowModel => {
          val newDataFlow = SourcePathInfo(dataflowModel.sourceId, dataflowModel.sinkId, dataflowModel.pathId)
          if (!flowPathBeforeSecondDedup.contains(newDataFlow)) {
            flowPathBeforeSecondDedup += newDataFlow
          }
        })
      })
    })
  }

  def checkFlowExistInSecondDedup(sourcePathInfo: SourcePathInfo): Boolean =
    flowPathBeforeSecondDedup.contains(sourcePathInfo)

  def addIntoFinalPath(dataFlowPathModel: List[DataFlowPathModel]): Unit = {
    dataFlowPathModel.foreach(flow => {
      flowPathFinal += SourcePathInfo(flow.sourceId, flow.sinkId, flow.pathId)
    })
  }

  def checkFlowExistInFinal(sourcePathInfo: SourcePathInfo): Boolean = flowPathFinal.contains(sourcePathInfo)

  case class SourcePathInfo(sourceId: String, sinkId: String, pathId: String) {
    override def equals(other: Any): Boolean = other match {
      case that: SourcePathInfo =>
        this.sourceId == that.sourceId && this.sinkId == that.sinkId && this.pathId == that.pathId
      case _ => false
    }

    override def hashCode: Int = sourceId.hashCode + sinkId.hashCode + pathId.hashCode
  }

}
