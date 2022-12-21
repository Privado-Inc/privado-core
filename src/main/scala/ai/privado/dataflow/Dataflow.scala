/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.dataflow

import ai.privado.cache.{AppCache, DataFlowCache}
import ai.privado.entrypoint.ScanProcessor
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.language.{Path, _}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.util.{Failure, Success}

class Dataflow(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  implicit val engineContext: EngineContext =
    EngineContext(semantics = Utilities.getSemantics(cpg), config = EngineConfig(4))

  /** Compute the flow of data from tagged Sources to Sinks
    * @return
    *   \- Map of PathId -> Path corresponding to source to sink path
    */
  def dataflow: Map[String, Path] = {

    logger.info("Generating dataflow")
    val sources = getSources
    val sinks   = getSinks

    if (sources.isEmpty || sinks.isEmpty)
      Map[String, Path]()
    else {
      val dataflowPathsUnfiltered = sinks.reachableByFlows(sources).l
      AppCache.totalFlowFromReachableBy = dataflowPathsUnfiltered.size
      val dataflowPaths = {
        if (ScanProcessor.config.disableThisFiltering)
          dataflowPathsUnfiltered
        else
          dataflowPathsUnfiltered
            .filter(DuplicateFlowProcessor.filterFlowsByContext)
            .filter(DuplicateFlowProcessor.flowNotTaintedByThis)
      }
      AppCache.totalFlowAfterThisFiltering = dataflowPaths.size
      // Stores key -> PathID, value -> Path
      val dataflowMapByPathId = dataflowPaths
        .flatMap(dataflow => {
          DuplicateFlowProcessor.calculatePathId(dataflow) match {
            case Success(pathId) => Some(pathId, dataflow)
            case Failure(e) =>
              logger.debug("Exception : ", e)
              None
          }
        })
        .toMap

      // Setting cache
      DataFlowCache.dataflowsMapByType = dataflowMapByPathId
      DuplicateFlowProcessor.filterIrrelevantFlowsAndStoreInCache(dataflowMapByPathId)
      // Need to return the filtered result
      val dataflowFromCache = DataFlowCache.getDataflow
      dataflowFromCache.map(_.pathId).toSet.map((pathId: String) => (pathId, dataflowMapByPathId(pathId))).toMap
    }
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
      .l ++ cpg.argument.isFieldIdentifier.where(filterSources).l

  }

  private def getSinks: List[CfgNode] = {
    cpg.call.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l
  }

}
