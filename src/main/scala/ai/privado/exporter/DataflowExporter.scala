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

package ai.privado.exporter

import ai.privado.cache.{DataFlowCache, RuleCache}
import ai.privado.model.{Constants, DataFlowPathModel, NodeType}
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DataflowExporter(cpg: Cpg, dataflowsMap: Map[String, Path]) {

  val falsePositiveSources = List[String](
    "Data.Sensitive.OnlineIdentifiers.Cookies",
    "Data.Sensitive.OnlineIdentifiers.IPAddress",
    "Data.Sensitive.PersonalCharacteristics.Signature",
    "Data.Sensitive.BiometricData.FingerprintScans"
  )

  val logger = LoggerFactory.getLogger(getClass)

  def getFlowByType(sinkSubCategory: String, sinkNodeTypes: Set[String]) = {
    sinkNodeTypes.flatMap(sinkNodeType => {
      val dataflowModelFilteredByType = DataFlowCache.getDataflow.filter(dataflowModel =>
        dataflowModel.sinkSubCategory.equals(sinkSubCategory) && dataflowModel.sinkNodeType.equals(sinkNodeType)
      )
      val dataflowModelBySourceId = dataflowModelFilteredByType.groupBy(_.sourceId)
      val dataflowOutputList      = ListBuffer[mutable.LinkedHashMap[String, Json]]()
      dataflowModelBySourceId.foreach(dataflowBySourceEntrySet => {
        val dataflowOutput = mutable.LinkedHashMap[String, Json]()
        dataflowOutput.addOne(Constants.sourceId -> dataflowBySourceEntrySet._1.asJson)
        dataflowOutput.addOne(
          Constants.sinks -> convertSourceModelList(dataflowBySourceEntrySet._2, sinkSubCategory, sinkNodeType)
        )
        dataflowOutputList += dataflowOutput
      })
      dataflowOutputList
    })
  }

  def convertSourceModelList(
    sourceModelList: List[DataFlowPathModel],
    sinkSubCategory: String,
    sinkNodeType: String
  ): Json = {
    def convertSink(sinkId: String, sinkPathIds: ListBuffer[String]) = {
      var sinkOutput       = mutable.LinkedHashMap[String, Json]()
      val sinkIdAfterSplit = sinkId.split("#_#")
      sinkOutput.addOne(Constants.sinkType -> sinkSubCategory.asJson)
      sinkOutput = sinkOutput ++ ExporterUtility.getRuleInfoForExporting(sinkIdAfterSplit(0))
      // Special case for API type of nodes
      RuleCache.getRuleInfo(sinkIdAfterSplit(0)) match {
        case Some(rule) if rule.nodeType.equals(NodeType.API) & sinkIdAfterSplit.size >= 2 =>
          sinkOutput.addOne(Constants.apiUrl -> sinkIdAfterSplit(1).split(",").toList.asJson)
        case _ => // do nothing
      }
      sinkOutput
        .addOne(
          Constants.paths -> sinkPathIds
            .map(sinkPathId => convertPathsList(dataflowsMap(sinkPathId), sinkPathId))
            .asJson
        )
        .asJson
      sinkOutput
    }

    // sinkMap will have (sinkId -> List[String]() where value are all the paths/grouping-of-path which belong to the sinkId
    val sinkMap = mutable.HashMap[String, ListBuffer[String]]()
    sourceModelList.foreach(sourceModel => {
      var sinkId = sourceModel.sinkId
      val sinkAPITag = dataflowsMap(sourceModel.pathId).elements.last.tag
        .filter(node => node.name.equals(Constants.apiUrl))
      if (sinkAPITag.nonEmpty) {
        sinkId += "#_#" + sinkAPITag.value.l.mkString(",")
      }
      if (!sinkMap.contains(sinkId))
        sinkMap(sinkId) = ListBuffer()
      sinkMap(sinkId).append(sourceModel.pathId)
    })
    sinkMap.map(entrySet => convertSink(entrySet._1, entrySet._2)).asJson

  }

  private def convertPathsList(sinkFlow: Path, pathId: String) = {
    val pathOutput = mutable.LinkedHashMap[String, Json]()

    pathOutput.addOne(Constants.pathId -> pathId.asJson)
    pathOutput.addOne(Constants.path   -> ExporterUtility.convertPathElements(sinkFlow.elements).asJson)
    pathOutput
  }

}
