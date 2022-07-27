package ai.privado.dataflow

import ai.privado.model.{Constants, NodeType}
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode}

class Dataflow(cpg: Cpg) {

  def dataflow: Traversal[Path] = {

    val sources = getSources
    val sinks   = getSinks

    println(s"length of sources : ${sources.length}")
    println(s"length of sinks : ${sinks.length}")

    implicit val engineContext: EngineContext = EngineContext(Utilities.getDefaultSemantics())
    sinks.reachableByFlows(sources)
  }

  private def getSources: List[CfgNode] = {
    cpg.literal
      .where(
        _.tag
          .nameExact(Constants.nodeType)
          .or(_.valueExact(NodeType.SOURCE.toString), _.valueExact(NodeType.DERIVED_SOURCE.toString))
      )
      .l ++ cpg.identifier
      .where(
        _.tag
          .nameExact(Constants.nodeType)
          .or(_.valueExact(NodeType.SOURCE.toString), _.valueExact(NodeType.DERIVED_SOURCE.toString))
      )
      .l ++ cpg.call
      .where(
        _.tag
          .nameExact(Constants.nodeType)
          .or(_.valueExact(NodeType.SOURCE.toString), _.valueExact(NodeType.DERIVED_SOURCE.toString))
      )
      .l

  }

  private def getSinks: List[Call] = {
    cpg.call
      .or(
        _.tag.nameExact(Constants.nodeType).valueExact(NodeType.API.toString),
        _.tag.nameExact(Constants.nodeType).valueExact(NodeType.DATABASE.toString),
        _.tag.nameExact(Constants.nodeType).valueExact(NodeType.LEAKAGE.toString),
        _.tag.nameExact(Constants.nodeType).valueExact(NodeType.SDK.toString)
      )
      .l
  }
}
