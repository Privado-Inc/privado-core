/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.dataflow

import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.semantic.Language._
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class Dataflow(cpg: Cpg) {

  private val logger = LoggerFactory.getLogger(getClass)
  implicit val engineContext: EngineContext = EngineContext(Utilities.getDefaultSemantics(cpg))
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
