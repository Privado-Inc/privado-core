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

import ai.privado.dataflow.DuplicateFlowProcessor
import ai.privado.entrypoint.ScanProcessor
import ai.privado.model.exporter.{
  DataFlowPathIntermediateModel,
  DataFlowSinkIntermediateModel,
  DataFlowSourceIntermediateModel,
  DataFlowSubCategoryPathIntermediateModel
}
import ai.privado.model.DataFlowPathModel
import ai.privado.semantic.Language.finder
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.semanticcpg.language._

import java.util.Calendar
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DataFlowCache {

  var dataflowsMapByType: Map[String, Path] = Map[String, Path]()

  val dataflow = mutable.HashMap[String, mutable.HashMap[String, ListBuffer[DataFlowPathModel]]]()

  lazy val finalDataflow: List[DataFlowPathModel] = {
    val extraFlows = {
      if (!ScanProcessor.config.disableDeDuplication)
        setDataflowWithdedupAndReturnDataflowsWithApplyDedupFalse()
      else
        List()
    }
    dataflow.flatMap(_._2.values.flatMap(_.toList)).toList ::: extraFlows
  }

  var intermediateDataFlow: List[DataFlowPathIntermediateModel] = List[DataFlowPathIntermediateModel]()

  def setDataflow(dataFlowPathModel: DataFlowPathModel): Unit = {

    val pathId               = dataFlowPathModel.pathId
    val sinkNodeWithLocation = dataflowsMapByType(pathId).elements.last.location
    val fileLineNo =
      sinkNodeWithLocation.lineNumber.getOrElse(0).toString + sinkNodeWithLocation.filename + dataFlowPathModel.sinkId
    val sourceId = dataFlowPathModel.sourceId

    if (!dataflow.contains(sourceId)) {
      dataflow(sourceId) = new mutable.HashMap().addOne(fileLineNo, ListBuffer())
    } else if (!dataflow(sourceId).contains(fileLineNo)) {
      dataflow(sourceId)(fileLineNo) = ListBuffer()
    }

    dataflow(sourceId)(fileLineNo).append(dataFlowPathModel)
  }

  def getDataflow: List[DataFlowPathModel] = finalDataflow

  def getIntermediateDataFlow(): List[DataFlowSourceIntermediateModel] = {

    // Translating into json output format structure
    val intermediateSourceResult     = ListBuffer[DataFlowSourceIntermediateModel]()
    val intermediateDataFlowBySource = intermediateDataFlow.groupBy(_.sourceId)
    intermediateDataFlowBySource.map(entrySet => {
      val intermediateDataFlowBySink = entrySet._2.groupBy(_.sinkId)
      val intermediateSinkResult     = ListBuffer[DataFlowSinkIntermediateModel]()
      intermediateDataFlowBySink.map(pathValue => {
        val intermediatePathResult = ListBuffer[DataFlowSubCategoryPathIntermediateModel]()
        pathValue._2.foreach(value => {
          intermediatePathResult += DataFlowSubCategoryPathIntermediateModel(value.pathId, value.paths)
        })
        intermediateSinkResult += DataFlowSinkIntermediateModel(pathValue._1, intermediatePathResult.toList)
      })
      intermediateSourceResult += DataFlowSourceIntermediateModel(entrySet._1, intermediateSinkResult.toList)
    })
    intermediateSourceResult.toList
  }

  private def setDataflowWithdedupAndReturnDataflowsWithApplyDedupFalse() = {

    println(s"${Calendar.getInstance().getTime} - Deduplicating data flows...")
    def addToMap(dataFlowPathModel: DataFlowPathModel): Unit = {

      val pathId               = dataFlowPathModel.pathId
      val sinkNodeWithLocation = dataflowsMapByType(pathId).elements.last.location
      val fileLineNo =
        sinkNodeWithLocation.lineNumber.getOrElse(0).toString + sinkNodeWithLocation.filename + dataFlowPathModel.sinkId
      val flowSize = dataflowsMapByType(pathId).elements.size
      val sourceId = dataFlowPathModel.sourceId

      if (!dataflow.contains(sourceId)) {
        dataflow(sourceId) = new mutable.HashMap().addOne(fileLineNo, ListBuffer())
      } else if (!dataflow(sourceId).contains(fileLineNo)) {
        dataflow(sourceId)(fileLineNo) = ListBuffer()
      }

      if (dataflow(sourceId)(fileLineNo).nonEmpty) {
        val currentDataFlowPathModel    = dataflow(sourceId)(fileLineNo).head
        val currentPathId               = currentDataFlowPathModel.pathId
        val currentSinkNodeWithLocation = dataflowsMapByType(currentPathId).elements.last.location

        val currentFileLineNo =
          currentSinkNodeWithLocation.lineNumber
            .getOrElse(0)
            .toString + currentSinkNodeWithLocation.filename + currentDataFlowPathModel.sinkId
        val currentFlowSize = dataflowsMapByType(dataflow(sourceId)(fileLineNo).head.pathId).elements.size
        if (currentFileLineNo.equals(fileLineNo) && flowSize < currentFlowSize) {
          dataflow(sourceId)(fileLineNo) = ListBuffer[DataFlowPathModel](dataFlowPathModel)
        }
      } else {
        dataflow(sourceId)(fileLineNo) = ListBuffer[DataFlowPathModel](dataFlowPathModel)
      }

    }

    val filteredSourceIdMap = dataflow.map(entrySet => {
      val sourceId = entrySet._1
      // Consider only the flows for which applyDedup is true
      val pathIdsWithAppyDedupTrue = entrySet._2.values.flatMap(_.filter(_.applyDedup).map(_.pathId).toList).toSet
      val filteredPathIds          = DuplicateFlowProcessor.pathIdsPerSourceIdAfterDedup(pathIdsWithAppyDedupTrue)

      val filteredFileLineNumberMap = entrySet._2.map(fileLineNoEntry => {
        (fileLineNoEntry._1, fileLineNoEntry._2.filter(dfpm => filteredPathIds.contains(dfpm.pathId)))
      })
      (sourceId, filteredFileLineNumberMap)
    })
    val flowsWithAppyDataflowFalse = dataflow.flatMap(_._2.values.flatMap(_.filterNot(_.applyDedup).toList)).toList
    // clear the content and set fresh content
    dataflow.clear()
    filteredSourceIdMap.foreach(sourceMap => {
      sourceMap._2.foreach(fileLineNoEntry => {
        fileLineNoEntry._2.foreach(dfpm => addToMap(dfpm))
      })
    })
    flowsWithAppyDataflowFalse
  }

}
