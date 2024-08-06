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

import ai.privado.cache.{AppCache, DataFlowCache, RuleCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.model.exporter.{DataFlowSubCategoryModel, SourceModel, SourceProcessingModel}
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import ai.privado.tagger.utility.SourceTaggerUtility.getFilteredSourcesByTaggingDisabled
import ai.privado.utility.Utilities
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Tag}
import ai.privado.semantic.Language.*
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.Traversal
import io.joern.dataflowengineoss.language.Path

import scala.collection.mutable

class SourceExporter(
  cpg: Cpg,
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  repoItemTagName: Option[String] = None,
  appCache: AppCache
) {

  lazy val sourcesList: List[AstNode]      = getSourcesList
  lazy val sourcesTagList: List[List[Tag]] = sourcesList.map(_.tag.l)

  /** Fetch and Convert sources to desired output
    */
  def getSources: List[SourceModel] = {
    convertSourcesList(sourcesTagList)
  }

  def getProcessing(
    dataflowsOutput: mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]],
    dataflows: Map[String, Path]
  ): List[SourceProcessingModel] = {

    val processingMap = mutable.HashMap[String, mutable.Set[AstNode]]()
    sourcesList.foreach(source => {
      def addToMap(sourceId: String): Unit = {
        if (processingMap.contains(sourceId)) {
          processingMap(sourceId) = processingMap(sourceId).addOne(source)
        } else {
          processingMap.addOne(sourceId -> mutable.Set(source))
        }
      }
      source.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
      source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
    })
    processingMap
      .map(entrySet =>
        // list of tagged source node
        val taggedSourcesList = {
          if (privadoInput.disableDeDuplication)
            entrySet._2.toList
          else
            entrySet._2.toList
              .distinctBy(_.code)
              .distinctBy(_.lineNumber)
              .distinctBy(Utilities.getFileNameForNode)
        }
        // list first element of every dataflow
        val dataflowSourceList = dataflowsOutput
          .flatMap(_._2)
          .filter(_.sourceId.equals(entrySet._1))
          .flatMap(_.sinks)
          .flatMap(_.paths)
          .map(path => dataflows(path.pathId).elements.head)
        val finalProcessingResultSet = taggedSourcesList.toSet ++ dataflowSourceList.toSet
        SourceProcessingModel(
          entrySet._1,
          ExporterUtility
            .convertPathElements(finalProcessingResultSet.toList, appCache = appCache, ruleCache = ruleCache)
        )
      )
      .toList
  }

  /** Fetch all the sources node
    */
  private def getSourcesList: List[AstNode] = {
    def filterSource(traversal: Traversal[AstNode]) = {

      if (privadoInput.isMonolith) {
        traversal
          .where(
            _.tag
              .nameExact(Constants.catLevelOne)
              .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
          )
      } else {
        traversal
          .where(
            _.tag
              .nameExact(Constants.catLevelOne)
              .valueExact(CatLevelOne.SOURCES.name)
          )
      }
    }
    val sources: List[AstNode] =
      cpg.identifier
        .where(filterSource)
        .l ++
        cpg.literal
          .where(filterSource)
          .l ++
        cpg.call
          .where(filterSource)
          .l ++
        cpg.templateDom
          .where(filterSource)
          .l ++ cpg.argument.isFieldIdentifier.where(filterSource).l ++ cpg.member
          .where(filterSource)
          .l ++ cpg.sqlColumn
          .where(filterSource)
          .l ++ cpg.androidXmlPermissionNode.where(filterSource).l

    ExporterUtility.filterNodeBasedOnRepoItemTagName(
      // Remove TAGGING_DISABLED_BY_DED sources from list
      getFilteredSourcesByTaggingDisabled(sources),
      repoItemTagName
    )
  }

  private def convertSourcesList(sources: List[List[Tag]]): List[SourceModel] = {
    def convertSource(sourceId: String) = {
      ruleCache.getRuleInfo(sourceId) match {
        case Some(rule) =>
          val ruleInfoExporterModel = ExporterUtility.getRuleInfoForExporting(ruleCache, sourceId)
          Some(
            SourceModel(
              rule.catLevelOne.label,
              ruleInfoExporterModel.id,
              ruleInfoExporterModel.name,
              ruleInfoExporterModel.category,
              ruleInfoExporterModel.sensitivity,
              ruleInfoExporterModel.isSensitive,
              ruleInfoExporterModel.tags
            )
          )
        case None => // not found anything, probably derived source
          None
      }
    }

    def getSources(nodeList: List[Tag]) = {
      val node = nodeList
        .filterNot(node => InternalTag.valuesAsString.contains(node.name))
        .filter(node => node.name.equals(Constants.id) || node.name.startsWith(Constants.privadoDerived))
      node.value.toSet
    }

    sources
      .flatMap(getSources)
      .toSet
      .flatMap(convertSource)
      .toList
  }

}
