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

package ai.privado.cache

import ai.privado.entrypoint.ScanProcessor
import ai.privado.model.DataFlowPathModel
import ai.privado.semantic.Language.finder
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DataFlowCache {

  val dataflow = mutable.HashMap[String, mutable.HashMap[String, ListBuffer[DataFlowPathModel]]]()

  def setDataflow(dataFlowPathModel: DataFlowPathModel): Unit = {

    val pathId               = dataFlowPathModel.pathId
    val dataflowsMapByType   = dataFlowPathModel.dataflowsMapByType
    val sinkNodeWithLocation = dataflowsMapByType(pathId).elements.last.location
    val fileLineNo           = sinkNodeWithLocation.lineNumber.getOrElse(0).toString + sinkNodeWithLocation.filename
    val flowSize             = dataflowsMapByType(pathId).elements.size
    val sourceId             = dataFlowPathModel.sourceId

    if (!dataflow.contains(sourceId)) {
      dataflow(sourceId) = new mutable.HashMap().addOne(fileLineNo, ListBuffer())
    } else if (!dataflow(sourceId).contains(fileLineNo)) {
      dataflow(sourceId)(fileLineNo) = ListBuffer()
    }

    if (ScanProcessor.config.disableDeDuplication) {
      dataflow(sourceId)(fileLineNo).append(dataFlowPathModel)
    } else {
      if (dataflow(sourceId)(fileLineNo).nonEmpty) {
        val currentPathId               = dataflow(sourceId)(fileLineNo).head.pathId
        val currentSinkNodeWithLocation = dataflowsMapByType(currentPathId).elements.last.location

        val currentFileLineNo =
          currentSinkNodeWithLocation.lineNumber.getOrElse(0).toString + currentSinkNodeWithLocation.filename
        val currentFlowSize = dataflowsMapByType(dataflow(sourceId)(fileLineNo).head.pathId).elements.size
        if (currentFileLineNo.equals(fileLineNo) && flowSize < currentFlowSize) {
          dataflow(sourceId)(fileLineNo) = ListBuffer[DataFlowPathModel](dataFlowPathModel)
        }
      } else {
        dataflow(sourceId)(fileLineNo) = ListBuffer[DataFlowPathModel](dataFlowPathModel)
      }
    }
  }

  def getDataflow: List[DataFlowPathModel] = dataflow.values.flatMap(_.values).flatten.toList

}
