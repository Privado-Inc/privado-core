package ai.privado.exporter

import ai.privado.model.{Constants, InternalTags, NodeType}
import ai.privado.model.NodeType.NodeType
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import ExporterUtility._

class DataflowExporter(cpg: Cpg, dataflowsMap: Map[String, Path]) {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  def getFlowByType(nodeType: NodeType, dataflowSinkType: String) = {

    val dataflowsMapByType = dataflowsMap.filter(dataflowEntrySet =>
      dataflowEntrySet._2.elements.last
        .where(_.tag.nameExact(Constants.nodeType).valueExact(nodeType.toString))
        .nonEmpty
    )

    val dataflowsMapBySourceId = mutable.HashMap[String, ListBuffer[String]]()
    dataflowsMapByType.foreach(entrySet => {
      def addToMap(sourceId: String) = {
        if (dataflowsMapBySourceId.contains(sourceId))
          dataflowsMapBySourceId(sourceId) += entrySet._1
        else
          dataflowsMapBySourceId.addOne(sourceId, ListBuffer(entrySet._1))
      }
      val source = entrySet._2.elements.head
      if (source.tag.nameExact(Constants.nodeType).value.head.equals(NodeType.SOURCE.toString)) {
        addToMap(source.tag.nameExact(Constants.id).l.head.value)
      } else {
        source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
      }
    })

    val dataflowOutputList = ListBuffer[mutable.LinkedHashMap[String, Json]]()

    dataflowsMapBySourceId.foreach(flow => {
      val dataflowOutput = mutable.LinkedHashMap[String, Json]()
      dataflowOutput.addOne(Constants.sourceId -> flow._1.asJson)
      dataflowOutput.addOne(Constants.sinks -> convertSinksList(flow._2.toList, dataflowsMapByType, dataflowSinkType))

      dataflowOutputList += dataflowOutput
    })

    dataflowOutputList
  }

  private def convertSinksList(
    sinkFlows: List[String],
    dataflowsMapByType: Map[String, Path],
    dataflowSinkType: String
  ) = {

    def convertSink(sinkFlow: Path, sinkFlowId: String) = {
      val sinkMap = mutable.HashMap[String, Json]()
      val tagMap  = mutable.HashMap[String, String]()
      sinkFlow.elements.last.tag
        .filterNot(node => InternalTags.valuesAsString.contains(node.name))
        .foreach(node => {
          node match {
            case x if x.name.equals(Constants.id) || x.name.equals(Constants.name) =>
              sinkMap.addOne(node.name, node.value.asJson)
            case x if x.name.equals(Constants.nodeType) =>
              sinkMap.addOne(Constants.sinkType -> dataflowSinkType.asJson)
            case _ => tagMap.addOne(node.name, node.value)
          }
        })
      sinkMap.addOne(Constants.tags  -> tagMap.asJson)
      sinkMap.addOne(Constants.paths -> convertPathsList(sinkFlow, sinkFlowId).asJson)

      val sinkOutput = mutable.LinkedHashMap[String, Json]()
      addElementFromMapToOrderedMap(sinkOutput, sinkMap, Constants.sinkType)
      addElementFromMapToOrderedMap(sinkOutput, sinkMap, Constants.id)
      addElementFromMapToOrderedMap(sinkOutput, sinkMap, Constants.name)
      addElementFromMapToOrderedMap(sinkOutput, sinkMap, Constants.tags)
      addElementFromMapToOrderedMap(sinkOutput, sinkMap, Constants.paths)

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
