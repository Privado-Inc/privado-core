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
 */

package ai.privado.dataflow

import ai.privado.model.Constants
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

object DuplicateFlowProcessor {

  private val NODE_PATH_SEPARATOR = "-"

  private val logger = LoggerFactory.getLogger(getClass)

  /** Process the given dataflows and return only the distinct one
    *
    * What do we want - 8 Flows
    *
    * Source 1 to Sink 1, 2
    *
    * Source 2 to Sink 1, 2
    *
    * Source 3 to Sink 1, 2
    *
    * Source 4 to Sink 1, 2
    *
    * Algorithmic idea
    *
    *   1. Sort by length in descending order
    *
    * 2. for each path
    *
    * 2.1. calculate id 2.2. check for distinct (set) 2.2.1. if not distinct, discard 2.3. add all possible subpaths to
    * set
    *
    * @param dataflows
    * @return
    *   Unique dataflows with PathId
    */
  def process(dataflows: List[Path]): Map[String, Path] = {
    // Stores pathId -> Path
    val dataflowMap = dataflows.map(path => (calculatePathId(path).getOrElse(""), path)).toMap
    // Stores sourceId -> Set(pathIds)
    val dataflowMapBySourceId = mutable.HashMap[String, mutable.Set[String]]()
    dataflowMap.foreach(dataflowEntry => {
      def addToMap(sourceId: String) = {
        if (!dataflowMapBySourceId.contains(sourceId))
          dataflowMapBySourceId.addOne(sourceId -> mutable.Set())
        dataflowMapBySourceId(sourceId).add(dataflowEntry._1)
      }
      val sourceNode = dataflowEntry._2.elements.head
      sourceNode.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
      sourceNode.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
    })
    dataflowMapBySourceId
      .flatMap(dataflowMapBySourceIdEntrySet => pathIdsPerSourceIdAfterDedup(dataflowMapBySourceIdEntrySet._2))
      .toSet
      .map((pathId: String) => (pathId, dataflowMap(pathId)))
      .toMap
  }

  /** Filter unique path ids which are super set of overlapping paths
    *
    * @param pathIds
    * @return
    */
  private def pathIdsPerSourceIdAfterDedup(pathIds: mutable.Set[String]) = {
    val visitedFlows = mutable.Set[String]()
    pathIds.foreach(pathId => {
      if (!visitedFlows.contains(pathId)) {
        val pathSubIds = getSubPathIds(pathId)
        if (pathSubIds.nonEmpty)
          visitedFlows.addAll(pathSubIds)
      }
    })
    pathIds.diff(visitedFlows) // This will give us all the Path ids which are super set of overlapping paths
  }

  /** Generates a pathId for a given path, based on node Id
    * @param flow
    * @return
    */
  def calculatePathId(flow: Path) = Try {
    flow.elements.map(node => node.id()).mkString(NODE_PATH_SEPARATOR)
  }

  /** Returns all the sub pathIds for a given path Id Ex - For path Id - 121-23-47-1999-2143-8 SubpathIds are -
    * 23-47-1999-2143-8, 47-1999-2143-8, 1999-2143-8, 2143-8
    * @param pathId
    * @return
    */
  private def getSubPathIds(pathId: String) = {
    val subIdList = ListBuffer[String]()
    val pathIds   = pathId.split(NODE_PATH_SEPARATOR)
    for (i <- 1 until (pathIds.size - 1)) {
      subIdList.append(pathIds.slice(i, pathIds.size).mkString(NODE_PATH_SEPARATOR))
    }
    subIdList.toList
  }
}
