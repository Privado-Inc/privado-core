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

import ai.privado.cache.{DataFlowCache, DatabaseDetailsCache, RuleCache, TaggerCache}
import ai.privado.model.exporter.{DataFlowSubCategoryModel, DataFlowSubCategoryPathModel, DataFlowSubCategorySinkModel}
import ai.privado.model.{Constants, DataFlowPathModel, DatabaseDetails, NodeType}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DataflowExporter(cpg: Cpg, dataflowsMap: Map[String, Path], taggerCache: TaggerCache) {

  val falsePositiveSources: List[String] = List[String](
    "Data.Sensitive.OnlineIdentifiers.Cookies",
    "Data.Sensitive.OnlineIdentifiers.IPAddress",
    "Data.Sensitive.PersonalCharacteristics.Signature",
    "Data.Sensitive.BiometricData.FingerprintScans"
  )

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def getFlowByType(
    sinkSubCategory: String,
    sinkNodeTypes: Set[String],
    ruleCache: RuleCache
  ): Set[DataFlowSubCategoryModel] = {
    val dataflowModelFilteredByType = DataFlowCache.getDataflow.filter(dataflowModel =>
      dataflowModel.sinkSubCategory.equals(sinkSubCategory) && sinkNodeTypes.contains(dataflowModel.sinkNodeType)
    )
    val dataflowModelBySourceId = dataflowModelFilteredByType.groupBy(_.sourceId)
    dataflowModelBySourceId
      .map(dataflowBySourceEntrySet => {
        DataFlowSubCategoryModel(
          dataflowBySourceEntrySet._1,
          convertSourceModelList(dataflowBySourceEntrySet._1, dataflowBySourceEntrySet._2, sinkSubCategory, ruleCache)
        )
      })
      .toSet
  }

  def convertSourceModelList(
    sourceId: String,
    sourceModelList: List[DataFlowPathModel],
    sinkSubCategory: String,
    ruleCache: RuleCache
  ): List[DataFlowSubCategorySinkModel] = {
    def convertSink(sourceId: String, sinkId: String, sinkPathIds: List[String], urls: mutable.Set[String]) = {

      // Special case for API type of nodes
      val apiUrl = if (urls.size > 0) urls.toList else List[String]()

      val databaseDetails = ruleCache.getRuleInfo(sinkId) match {
        case Some(rule) =>
          DatabaseDetailsCache.getDatabaseDetails(rule.id)
        case _ => Option.empty[DatabaseDetails]
      }

      val ruleInfo = ExporterUtility.getRuleInfoForExporting(ruleCache, sinkId)
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
        databaseDetails.getOrElse(DatabaseDetails("", "", "", "", "")),
        sinkPathIds
          .map(sinkPathId => convertPathsList(dataflowsMap(sinkPathId), sinkPathId, sourceId))
      )
    }

    // sinkMap will have (sinkId -> List[String]() where value are all the paths/grouping-of-path which belong to the sinkId
    val sinkMap     = mutable.HashMap[String, ListBuffer[String]]()
    val sinkApiUrls = mutable.HashMap[String, mutable.Set[String]]()
    sourceModelList.foreach(sourceModel => {
      var sinkId = sourceModel.sinkId
      val sinkAPITag = dataflowsMap(sourceModel.pathId).elements.last.tag
        .filter(node => node.name.equals(Constants.apiUrl + sourceModel.sinkId))
      if (!sinkApiUrls.contains(sinkId))
        sinkApiUrls(sinkId) = mutable.Set()
      for (url <- sinkAPITag)
        sinkApiUrls(sinkId).add(url.value)
      if (!sinkMap.contains(sinkId))
        sinkMap(sinkId) = ListBuffer()
      sinkMap(sinkId).append(sourceModel.pathId)
    })
    sinkMap
      .map(entrySet => convertSink(sourceId, entrySet._1, entrySet._2.toSet.toList, sinkApiUrls(entrySet._1)))
      .toList

  }

  private def convertPathsList(sinkFlow: Path, pathId: String, sourceId: String) = {
    DataFlowSubCategoryPathModel(pathId, ExporterUtility.convertPathElements(sinkFlow.elements, sourceId, taggerCache))
  }

}
