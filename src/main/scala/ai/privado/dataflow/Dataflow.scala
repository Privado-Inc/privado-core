package ai.privado.dataflow

import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.semantic.Language._
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class Dataflow(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  def dataflow: List[Path] = {

    logger.info("Generating dataflow")
    val sources = getSources
    val sinks   = getSinks

    if (sources.isEmpty || sinks.isEmpty)
      List[Path]()
    else
      sinks.reachableByFlows(sources).l
  }

  private def getSources: List[CfgNode] = {
    def filterSources(traversal: Traversal[StoredNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
    }
    cpg.literal
      .where(filterSources)
      .l ++ cpg.identifier
      .where(filterSources)
      .l ++ cpg.call
      .where(filterSources)
      .l

  }

  private def getSinks: List[CfgNode] = {
    cpg.call.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l
  }
}
