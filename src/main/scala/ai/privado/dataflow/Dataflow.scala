package ai.privado.dataflow

import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode}

class Dataflow(cpg: Cpg) {

  def dataflow: Option[Traversal[Path]] = {

    implicit val engineContext: EngineContext = EngineContext(Utilities.getDefaultSemantics)
    val sources                               = getSources
    val sinks                                 = getSinks

    if(sources.isEmpty || sinks.isEmpty)
      None
    else
      Some(sinks.reachableByFlows(sources))
  }

  private def getSources: List[CfgNode] = {
    cpg.literal
      .where(
        _.tag
          .nameExact(Constants.catLevelOne)
          .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
      )
      .l ++ cpg.identifier
      .where(
        _.tag
          .nameExact(Constants.catLevelOne)
          .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
      )
      .l ++ cpg.call
      .where(
        _.tag
          .nameExact(Constants.catLevelOne)
          .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
      )
      .l

  }

  private def getSinks: List[Call] = {
    cpg.call.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l
  }
}
