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

package ai.privado.exporter

import ai.privado.cache.{DataFlowCache, DatabaseDetailsCache, RuleCache}
import ai.privado.model.exporter.{DataFlowSubCategoryModel, DataFlowSubCategoryPathModel, DataFlowSubCategorySinkModel}
import ai.privado.model.{Constants, DataFlowPathModel, NodeType, DatabaseDetails}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DataflowExporter(cpg: Cpg, dataflowsMap: Map[String, Path]) {

  val falsePositiveSources: List[String] = List[String](
    "Data.Sensitive.OnlineIdentifiers.Cookies",
    "Data.Sensitive.OnlineIdentifiers.IPAddress",
    "Data.Sensitive.PersonalCharacteristics.Signature",
    "Data.Sensitive.BiometricData.FingerprintScans"
  )

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def getFlowByType(sinkSubCategory: String, sinkNodeTypes: Set[String]): Set[DataFlowSubCategoryModel] = {
    sinkNodeTypes.flatMap(sinkNodeType => {
      val dataflowModelFilteredByType = DataFlowCache.getDataflow.filter(dataflowModel =>
        dataflowModel.sinkSubCategory.equals(sinkSubCategory) && dataflowModel.sinkNodeType.equals(sinkNodeType)
      )
      val dataflowModelBySourceId = dataflowModelFilteredByType.groupBy(_.sourceId)
      dataflowModelBySourceId.map(dataflowBySourceEntrySet => {
        DataFlowSubCategoryModel(
          dataflowBySourceEntrySet._1,
          convertSourceModelList(dataflowBySourceEntrySet._1, dataflowBySourceEntrySet._2, sinkSubCategory)
        )
      })
    })
  }

  def convertSourceModelList(
    sourceId: String,
    sourceModelList: List[DataFlowPathModel],
    sinkSubCategory: String
  ): List[DataFlowSubCategorySinkModel] = {
    def convertSink(sourceId: String, sinkId: String, sinkPathIds: List[String]) = {
      val sinkIdAfterSplit = sinkId.split("#_#")

      // Special case for API type of nodes
      val apiUrl = RuleCache.getRuleInfo(sinkIdAfterSplit(0)) match {
        case Some(rule) if rule.nodeType.equals(NodeType.API) & sinkIdAfterSplit.size >= 2 =>
          sinkIdAfterSplit(1).split(",").toList
        case _ => List[String]()
      }

      val databaseDetails = RuleCache.getRuleInfo(sinkIdAfterSplit(0)) match {
        case Some(rule)
            if rule.id.matches(
              "Storages.SpringFramework.Jdbc.*|Sinks.Database.JPA.*|Storages.MongoDB.SpringFramework.*|Storages.SpringFramework.Jooq.*"
            ) =>
          DatabaseDetailsCache.getDatabaseDetails(rule.id)
        case _ => Option.empty[DatabaseDetails]
      }

      val ruleInfo = ExporterUtility.getRuleInfoForExporting(sinkIdAfterSplit(0))
      DataFlowSubCategorySinkModel(
        sinkSubCategory,
        ruleInfo.id,
        ruleInfo.name,
        ruleInfo.category,
        ruleInfo.domains,
        ruleInfo.sensitivity,
        ruleInfo.isSensitive,
        ruleInfo.tags,
        apiUrl,
        databaseDetails.getOrElse(DatabaseDetails("", "", "", "")),
        sinkPathIds
          .map(sinkPathId => convertPathsList(dataflowsMap(sinkPathId), sinkPathId, sourceId))
      )
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
    sinkMap.map(entrySet => convertSink(sourceId, entrySet._1, entrySet._2.toList)).toList

  }

  private def convertPathsList(sinkFlow: Path, pathId: String, sourceId: String) = {
    DataFlowSubCategoryPathModel(pathId, ExporterUtility.convertPathElements(sinkFlow.elements, sourceId))
  }

}
