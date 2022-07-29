package ai.privado.exporter

import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DataflowExporter(cpg: Cpg, dataflowsMap: Map[String, Path]) {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder
  val logger                               = LoggerFactory.getLogger(getClass)

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

        try {
          val source = entrySet._2.elements.head
          if (source.tag.nameExact(Constants.catLevelOne).value.head.equals(CatLevelOne.SOURCES.name)) {
            addToMap(source.tag.nameExact(Constants.id).l.head.value)
          } else {
            source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
          }
        } catch {
          case e: Exception => logger.error("Exception while traversing dataflow path")
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
    if (dataflowsMapByType.isEmpty)
      mutable.Seq[mutable.LinkedHashMap[String, Json]]()
    else
      processNonEmptyFlows(dataflowsMapByType)
  }

  private def convertSinksList(
    sinkFlows: List[String],
    dataflowsMapByType: Map[String, Path],
    dataflowSinkType: String
  ) = {

    def convertSink(sinkFlow: Path, sinkFlowId: String) = {
      var sinkOutput = mutable.LinkedHashMap[String, Json]()
      val sink = sinkFlow.elements.last.tag
        .filterNot(node => InternalTag.valuesAsString.contains(node.name))
        .filter(node => node.name.equals(Constants.id))
      if (sink.nonEmpty) {
        val ruleId = sink.head.value
        sinkOutput.addOne(Constants.sinkType -> dataflowSinkType.asJson)
        sinkOutput = sinkOutput ++ ExporterUtility.getRuleInfoForExporting(ruleId)
        sinkOutput.addOne(Constants.paths -> convertPathsList(sinkFlow, sinkFlowId).asJson)
      } else
        sinkOutput
    }

    sinkFlows.map(sinkFlowId => convertSink(dataflowsMapByType(sinkFlowId), sinkFlowId)).asJson
  }

  private def convertPathsList(sinkFlow: Path, pathId: String) = {
    val pathOutput = mutable.LinkedHashMap[String, Json]()

    pathOutput.addOne(Constants.pathId -> pathId.asJson)
    pathOutput.addOne(Constants.path   -> ExporterUtility.convertPathElement(sinkFlow.elements).asJson)
    pathOutput
  }

}
