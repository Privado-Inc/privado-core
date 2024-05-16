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

import ai.privado.cache.{AppCache, DatabaseDetailsCache, RuleCache, TaggerCache}
import ai.privado.model.exporter.{DataFlowSubCategoryModel, DataFlowSubCategoryPathModel, DataFlowSubCategorySinkModel}
import ai.privado.model.{Constants, DataFlowPathModel, DatabaseDetails}
import io.circe.Json
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}
import better.files.File
import ai.privado.model.exporter.DataFlowEncoderDecoder._
import io.circe.syntax.EncoderOps

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DataflowExporter(dataflowsMap: Map[String, Path], taggerCache: TaggerCache) {

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
    ruleCache: RuleCache,
    dataFlowModel: List[DataFlowPathModel],
    appCache: AppCache,
    extraFlowMap: Map[String, Path] = dataflowsMap
  ): Set[DataFlowSubCategoryModel] = {
    val dataflowModelFilteredByType = dataFlowModel.filter(dataflowModel =>
      dataflowModel.sinkSubCategory.equals(sinkSubCategory) && sinkNodeTypes.contains(dataflowModel.sinkNodeType)
    )
    val dataflowModelBySourceId = dataflowModelFilteredByType.groupBy(_.sourceId)
    dataflowModelBySourceId
      .map(dataflowBySourceEntrySet => {
        DataFlowSubCategoryModel(
          dataflowBySourceEntrySet._1,
          convertSourceModelList(
            dataflowBySourceEntrySet._1,
            dataflowBySourceEntrySet._2,
            sinkSubCategory,
            ruleCache,
            appCache,
            dataflowsMap ++ extraFlowMap
          )
        )
      })
      .toSet
  }

  def convertSourceModelList(
    sourceId: String,
    sourceModelList: List[DataFlowPathModel],
    sinkSubCategory: String,
    ruleCache: RuleCache,
    appCache: AppCache,
    dataflowMap: Map[String, Path] = dataflowsMap
  ): List[DataFlowSubCategorySinkModel] = {
    def convertSink(sourceId: String, sinkId: String, sinkPathIds: List[String], urls: Set[String]) = {

      // Special case for API type of nodes
      val apiUrl = if (urls.nonEmpty) urls.toList else List[String]()

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
          .map(sinkPathId => convertPathsList(dataflowMap(sinkPathId), sinkPathId, sourceId, appCache, ruleCache))
      )
    }

    // sinkMap will have (sinkId -> List[String]() where value are all the paths/grouping-of-path which belong to the sinkId
    val sinkMap     = mutable.HashMap[String, ListBuffer[String]]()
    val sinkApiUrls = mutable.HashMap[String, mutable.Set[String]]()
    sourceModelList.foreach(sourceModel => {
      var sinkId = sourceModel.sinkId
      val sinkAPITag = dataflowMap(sourceModel.pathId).elements.last.tag
        .filter(node => node.name.equals(Constants.apiUrl + sourceModel.sinkId))
      if (!sinkApiUrls.contains(sinkId))
        sinkApiUrls(sinkId) = mutable.Set()
      sinkApiUrls(sinkId).addAll(sinkAPITag.value.l)
      if (!sinkMap.contains(sinkId))
        sinkMap(sinkId) = ListBuffer()
      sinkMap(sinkId).append(sourceModel.pathId)
    })
    sinkMap
      .map(entrySet => convertSink(sourceId, entrySet._1, entrySet._2.toSet.toList, sinkApiUrls(entrySet._1).toSet))
      .toList

  }

  private def convertPathsList(
    sinkFlow: Path,
    pathId: String,
    sourceId: String,
    appCache: AppCache,
    ruleCache: RuleCache
  ) = {
    DataFlowSubCategoryPathModel(
      pathId,
      ExporterUtility.convertPathElements(sinkFlow.elements, sourceId, taggerCache, appCache, ruleCache)
    )
  }

}

object DataflowExporter {

  /** Function to limit the dataflows, in export
    * @param ruleCache
    * @param outputMap
    * @param repoPath
    */
  def limitDataflowsInExport(
    ruleCache: RuleCache,
    outputMap: mutable.LinkedHashMap[String, Json],
    repoPath: String
  ): Unit = {
    val elementInPathLimit: Int =
      ruleCache.getSystemConfigByKey(Constants.dataflowElementInPathLimit, true).toIntOption.getOrElse(-1)
    val sourceSinkPairPathLimit: Int =
      ruleCache.getSystemConfigByKey(Constants.dataflowSourceSinkPairPathLimit, true).toIntOption.getOrElse(-1)

    if (elementInPathLimit > 0 || sourceSinkPairPathLimit > 0) {
      val removedFlows = mutable.ListBuffer[DataFlowSubCategorySinkModel]()
      val alldataflows = outputMap(Constants.dataFlow)
        .as[mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]]
        .getOrElse(Map.empty[String, List[DataFlowSubCategoryModel]])
      val allnewDataflows = alldataflows
        .map((key, dataflows) => {
          val newDataflows = dataflows.map { subCatModel =>
            val subCatSinks = subCatModel.sinks.map { sink =>
              val sinkFilteredByElementInPathLimit =
                if (elementInPathLimit > 0 && !sink.paths.exists(_.path.size <= elementInPathLimit)) {
                  removedFlows.addOne(sink.copy(paths = sink.paths.drop(1)))
                  sink.copy(paths = sink.paths.take(1))
                } else sink

              val sinkFilteredBySourceSinkPairLimit = if (sourceSinkPairPathLimit > 0) {
                val sortedPaths = sinkFilteredByElementInPathLimit.paths.sortBy(_.path.size)
                removedFlows.addOne(
                  sinkFilteredByElementInPathLimit.copy(paths = sortedPaths.drop(sourceSinkPairPathLimit))
                )
                sinkFilteredByElementInPathLimit.copy(paths = sortedPaths.take(sourceSinkPairPathLimit))
              } else sinkFilteredByElementInPathLimit
              sinkFilteredBySourceSinkPairLimit
            }
            subCatModel.copy(sinks = subCatSinks)
          }
          (key, newDataflows)
        })
      outputMap.addOne(Constants.dataFlow -> allnewDataflows.asJson)
      val removedFlowsPath = s"$repoPath/${Constants.outputDirectoryName}/removedDataflows.json"
      File(removedFlowsPath).write(removedFlows.asJson.toString)
    }
  }
}
