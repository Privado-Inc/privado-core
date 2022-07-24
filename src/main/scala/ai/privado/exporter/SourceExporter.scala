package ai.privado.exporter

import ai.privado.model.{InternalTags, Constants, NodeType}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Tag}
import io.shiftleft.semanticcpg.language._
import io.circe._
import io.circe.syntax._
import scala.collection.mutable
import scala.collection.mutable.{HashMap, LinkedHashMap}
import ExporterUtility._

class SourceExporter(cpg: Cpg) {

  lazy val sourcesTagList = getSourcesTagList
  lazy val sourcesList    = getSourcesList

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder
  /*
    Fetch and Convert sources to desired output
   */
  def getSources = {
    convertSourcesList(sourcesTagList)
  }

  def getProcessing = {
    val processingMap = HashMap[String, List[CfgNode]]()
    sourcesList.foreach(source => {
      val sourceId = source.tag.nameExact(Constants.id).l.head.value
      if (processingMap.contains(sourceId)) {
        processingMap(sourceId) = processingMap(sourceId).+:(source)
      } else {
        processingMap.addOne(sourceId -> List(source))
      }
    })
    processingMap.map(entrySet =>
      mutable.LinkedHashMap[String, Json](
        Constants.sourceId    -> entrySet._1.asJson,
        Constants.occurrences -> ExporterUtility.convertPathElement(entrySet._2).asJson
      )
    )
  }

  /*
    Fetch all the sources tag
   */
  private def getSourcesTagList = {
    val sources =
      cpg.identifier
        .where(_.tag.nameExact(Constants.nodeType).valueExact(NodeType.SOURCE.toString))
        .map(item => item.tag.l)
        .l ++
        cpg.literal
          .where(_.tag.nameExact(Constants.nodeType).valueExact(NodeType.SOURCE.toString))
          .map(item => item.tag.l)
          .l ++
        cpg.call
          .where(_.tag.nameExact(Constants.nodeType).valueExact(NodeType.SOURCE.toString))
          .map(item => item.tag.l)
          .l
    sources
  }

  /*
    Fetch all the sources node
   */
  private def getSourcesList: List[CfgNode] = {
    val sources =
      cpg.identifier
        .where(_.tag.nameExact(Constants.nodeType).valueExact(NodeType.SOURCE.toString))
        .l ++
        cpg.literal
          .where(_.tag.nameExact(Constants.nodeType).valueExact(NodeType.SOURCE.toString))
          .l ++
        cpg.call
          .where(_.tag.nameExact(Constants.nodeType).valueExact(NodeType.SOURCE.toString))
          .l
    sources
  }

  def convertSourcesList(sources: List[List[Tag]]) = {
    def convertToStandardFormat(nodeList: List[Tag]) = {
      val tagMap    = new HashMap[String, String]()
      val sourceMap = new HashMap[String, Json]()
      nodeList
        .filterNot(node => InternalTags.valuesAsString.contains(node.name))
        .foreach(node => {
          node match {
            case x
                if x.name.equals(Constants.id) || x.name.equals(Constants.name) || x.name
                  .equals(Constants.category)
                  || x.name.equals(Constants.sensitivity) =>
              sourceMap.addOne(node.name, node.value.asJson)
            case x if x.name.equals(Constants.nodeType) =>
              sourceMap.addOne(Constants.sourceType, Constants.dataElementSource.asJson)
            case _ => tagMap.addOne(node.name, node.value)
          }
        })
      val orderedSourceMap = new LinkedHashMap[String, Json]()
      addElementFromMapToOrderedMap(orderedSourceMap, sourceMap, Constants.sourceType)
      addElementFromMapToOrderedMap(orderedSourceMap, sourceMap, Constants.id)
      addElementFromMapToOrderedMap(orderedSourceMap, sourceMap, Constants.name)
      addElementFromMapToOrderedMap(orderedSourceMap, sourceMap, Constants.category)
      addElementFromMapToOrderedMap(orderedSourceMap, sourceMap, Constants.sensitivity)
      orderedSourceMap.addOne(Constants.tags, tagMap.asJson)
      orderedSourceMap
    }
    sources.map(source => convertToStandardFormat(source)).toSet
  }

}
