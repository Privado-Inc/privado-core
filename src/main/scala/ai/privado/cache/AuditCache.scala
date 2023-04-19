package ai.privado.cache

import ai.privado.dataflow.DuplicateFlowProcessor
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{Constants, DataFlowPathModel, InternalTag}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.semanticcpg.language._
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

  def addIntoBeforeFirstFiltering(dataflowPathsUnfiltered: List[Path], privadoScanConfig: PrivadoInput, ruleCache: RuleCache): Unit = {

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

    this.dataflowMapByPathId = dataflowMapByPathId

    val sinkSubCategories = mutable.HashMap[String, mutable.Set[String]]()
    ruleCache.getRule.sinks.foreach(sinkRule => {
      if (!sinkSubCategories.contains(sinkRule.catLevelTwo))
        sinkSubCategories.addOne(sinkRule.catLevelTwo -> mutable.Set())
      sinkSubCategories(sinkRule.catLevelTwo).add(sinkRule.nodeType.toString)
    })

    sinkSubCategories.foreach(sinkSubTypeEntry => {
      val dataflowsMapByType = dataflowMapByPathId.filter(dataflowEntrySet =>
        dataflowEntrySet._2.elements.last
          .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(sinkSubTypeEntry._1))
          .nonEmpty
      )

      // Store sourceId -> List[PathIds] Paths which have sourceId as the source
      val dataflowsMapBySourceId = mutable.HashMap[String, ListBuffer[String]]()
      dataflowsMapByType.foreach(entrySet => {
        def addToMap(sourceId: String) = {
          if (!dataflowsMapBySourceId.contains(sourceId))
            dataflowsMapBySourceId.addOne(sourceId, ListBuffer())
          dataflowsMapBySourceId(sourceId) += entrySet._1
        }

        val source = entrySet._2.elements.head
        try {
          source.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
          if (privadoScanConfig.disable2ndLevelClosure)
            source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
          else
            source.tag.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString).value.foreach(addToMap)
        } catch {
          case e: Exception => logger.debug("Exception while fetching sourceId in dataflow : ", e)
        }
      })

      dataflowsMapBySourceId.foreach(flow => {
        processSinkListAndStoreInCache(
          flow._1,
          flow._2.toList,
          dataflowsMapByType,
          sinkSubTypeEntry._1,
          sinkSubTypeEntry._2.toSet
        )
      })
    })
  }

  private def processSinkListAndStoreInCache(
    pathSourceId: String,
    sinkPathIds: List[String],
    dataflowsMapByType: Map[String, Path],
    dataflowSinkType: String,
    dataflowNodeTypes: Set[String]
  ): Unit = {

    def addToCache(sinkPathId: String, dataflowNodeType: String): Unit = {
      val sinkCatLevelTwoCustomTag = dataflowsMapByType(sinkPathId).elements.last.tag
        .filter(node => node.name.equals(dataflowSinkType + dataflowNodeType))
      if (sinkCatLevelTwoCustomTag.nonEmpty) {
        sinkCatLevelTwoCustomTag.value.foreach(sinkId => {
          addIntoBeforeFirstFiltering(SourcePathInfo(pathSourceId, sinkId, sinkPathId))
        })
      }
    }

    sinkPathIds.foreach(sinkPathId => {
      dataflowNodeTypes.foreach(dataflowNodeTypes => {
        addToCache(sinkPathId, dataflowNodeTypes)
      })
    })
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

  def checkFlowExistinSecondDedup(sourcePathInfo: SourcePathInfo): Boolean =
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
