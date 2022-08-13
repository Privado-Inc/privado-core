package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, Constants, NodeType}
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DataflowExporter(cpg: Cpg, dataflowsMap: Map[String, Path]) {

  val logger = LoggerFactory.getLogger(getClass)

  def getFlowByType(sinkSubCategory: String): mutable.Seq[mutable.LinkedHashMap[String, Json]] = {
    def processNonEmptyFlows(
      dataflowsMapByType: Map[String, Path]
    ): mutable.Seq[mutable.LinkedHashMap[String, Json]] = {
      val dataflowsMapBySourceId = mutable.HashMap[String, ListBuffer[String]]()
      dataflowsMapByType.foreach(entrySet => {
        def addToMap(sourceId: String) = {
          if (dataflowsMapBySourceId.contains(sourceId))
            dataflowsMapBySourceId(sourceId) += entrySet._1
          else
            dataflowsMapBySourceId.addOne(sourceId, ListBuffer(entrySet._1))
        }

        val source = entrySet._2.elements.head
        try {
          source.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
          source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
        } catch {
          case e: Exception => logger.debug("Exception while traversing dataflow path : ", e)
        }
      })

      val dataflowOutputList = ListBuffer[mutable.LinkedHashMap[String, Json]]()

      dataflowsMapBySourceId.foreach(flow => {
        val dataflowOutput = mutable.LinkedHashMap[String, Json]()
        dataflowOutput.addOne(Constants.sourceId -> flow._1.asJson)
        dataflowOutput.addOne(Constants.sinks -> convertSinksList(flow._2.toList, dataflowsMapByType, sinkSubCategory))
        dataflowOutputList += dataflowOutput
      })
      dataflowOutputList
    }

    val dataflowsMapByType = dataflowsMap.filter(dataflowEntrySet =>
      dataflowEntrySet._2.elements.last
        .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(sinkSubCategory))
        .nonEmpty
    )
    MetricHandler.flowCategoryData(sinkSubCategory) = dataflowsMapByType.size
    if (dataflowsMapByType.isEmpty)
      mutable.Seq[mutable.LinkedHashMap[String, Json]]()
    else
      processNonEmptyFlows(dataflowsMapByType)
  }

  private def convertSinksList(
    sinkPathIds: List[String],
    dataflowsMapByType: Map[String, Path],
    dataflowSinkType: String
  ) = {

    def convertSink(sinkId: String, sinkPathIds: ListBuffer[String]) = {
      var sinkOutput       = mutable.LinkedHashMap[String, Json]()
      val sinkIdAfterSplit = sinkId.split("#_#")
      sinkOutput.addOne(Constants.sinkType -> dataflowSinkType.asJson)
      sinkOutput = sinkOutput ++ ExporterUtility.getRuleInfoForExporting(sinkIdAfterSplit(0))
      // Special case for API type of nodes
      RuleCache.getRuleInfo(sinkIdAfterSplit(0)) match {
        case Some(rule) if rule.nodeType.equals(NodeType.API) & sinkIdAfterSplit.size >= 2 =>
          sinkOutput.addOne(Constants.apiUrl -> sinkIdAfterSplit(1).split(",").toList.asJson)
        case _ => // do nothing
      }
      sinkOutput
        .addOne(
          Constants.paths -> sinkPathIds
            .map(sinkPathId => convertPathsList(dataflowsMapByType(sinkPathId), sinkPathId))
            .asJson
        )
        .asJson
      sinkOutput
    }

    // sinkMap will have (sinkId -> List[String]() where value are all the paths/grouping-of-path which belong to the sinkId
    val sinkMap = mutable.HashMap[String, ListBuffer[String]]()
    sinkPathIds.foreach(sinkPathId => {
      val sinkCatLevelTwoCustomTag = dataflowsMapByType(sinkPathId).elements.last.tag
        .filter(node => node.name.equals(dataflowSinkType))
      if (sinkCatLevelTwoCustomTag.nonEmpty) {
        var sinkId = sinkCatLevelTwoCustomTag.head.value
        val sinkAPITag = dataflowsMapByType(sinkPathId).elements.last.tag
          .filter(node => node.name.equals(Constants.apiUrl))
        if (sinkAPITag.nonEmpty) {
          sinkId += "#_#" + sinkAPITag.value.l.mkString(",")
        }
        if (!sinkMap.contains(sinkId))
          sinkMap(sinkId) = ListBuffer()
        sinkMap(sinkId).append(sinkPathId)
      }
    })
    sinkMap.map(entrySet => convertSink(entrySet._1, entrySet._2)).asJson
  }

  private def convertPathsList(sinkFlow: Path, pathId: String) = {
    val pathOutput = mutable.LinkedHashMap[String, Json]()

    pathOutput.addOne(Constants.pathId -> pathId.asJson)
    pathOutput.addOne(Constants.path   -> ExporterUtility.convertPathElements(sinkFlow.elements).asJson)
    pathOutput
  }

}
