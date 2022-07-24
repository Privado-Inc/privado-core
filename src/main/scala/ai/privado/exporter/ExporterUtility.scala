package ai.privado.exporter

import ai.privado.model.Constants
import ai.privado.utility.Utilities.dump
import io.circe.Json
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language.{DefaultNodeExtensionFinder, NodeExtensionFinder, toExtendedNode}

import scala.collection.mutable
import scala.collection.mutable.{HashMap, LinkedHashMap}

object ExporterUtility {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  /*
    Convert to path element schema object
   */
  def convertPathElement(nodes: List[CfgNode]) = {
    def converter(node: CfgNode) = {
      val occurrence   = mutable.LinkedHashMap[String, String]()
      val nodeLocation = node.location
      occurrence.addOne(Constants.sample -> nodeLocation.symbol)
      occurrence.addOne(Constants.lineNumber -> {
        nodeLocation.lineNumber match {
          case Some(n) => n.toString
          case None    => Constants.minusOne
        }
      })
      occurrence.addOne(Constants.columnNumber -> {
        node.columnNumber match {
          case Some(n) => n.toString
          case None    => Constants.minusOne
        }
      })
      occurrence.addOne(Constants.fileName -> nodeLocation.filename)

      occurrence.addOne(Constants.excerpt -> dump(nodeLocation.filename, node.lineNumber))
      occurrence
    }
    nodes.map(node => converter(node))
  }

  /*
  Add item from Map to ordered Map
   */
  def addElementFromMapToOrderedMap(
    orderedInputMap: LinkedHashMap[String, Json],
    inputMap: HashMap[String, Json],
    key: String
  ) = {
    if (inputMap.contains(key))
      orderedInputMap.addOne(key, inputMap.get(key).get)
  }

}
