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
import org.slf4j.LoggerFactory

class Dataflow(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  def dataflow: List[Path] = {

    implicit val engineContext: EngineContext = EngineContext(Utilities.getDefaultSemantics)
    logger.info("Generating dataflow")
    val sources = getSources
    val sinks   = getSinks

    if (sources.isEmpty || sinks.isEmpty)
      List[Path]()
    else
      sinks.reachableByFlows(sources).l
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
