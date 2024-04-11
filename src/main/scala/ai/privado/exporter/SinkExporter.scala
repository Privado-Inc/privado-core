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

import ai.privado.cache.{AppCache, DatabaseDetailsCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.model.exporter.{SinkModel, SinkProcessingModel}
import ai.privado.model.exporter.DataFlowEncoderDecoder.*
import ai.privado.semantic.Language.*
import ai.privado.model.{CatLevelOne, Constants, DatabaseDetails, InternalTag, NodeType}
import ai.privado.utility.Utilities
import io.shiftleft.codepropertygraph.generated.{Cpg, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode, Tag}
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.Traversal
import org.slf4j.LoggerFactory

import scala.collection.mutable

class SinkExporter(
  cpg: Cpg,
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  repoItemTagName: Option[String] = None,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache
) {

  lazy val sinkList: List[AstNode]      = getSinkList
  lazy val sinkTagList: List[List[Tag]] = sinkList.map(_.tag.l)

  private val logger = LoggerFactory.getLogger(getClass)

  /** Fetch and Convert sinks to desired output
    */
  def getSinks: List[SinkModel] = {
    convertSinkList(sinkTagList)
  }

  def getProcessing: List[SinkProcessingModel] = {
    val processingMap = mutable.HashMap[String, mutable.Set[AstNode]]()
    // special map to store sink processing which should never be deduplicated
    val processingMapDisableDedup = mutable.HashMap[String, mutable.Set[AstNode]]()
    sinkList.foreach(sink => {
      def addToMap(sinkId: String): Unit = {
        if (sinkId.startsWith(Constants.cookieWriteRuleId)) {
          if (!processingMapDisableDedup.contains(sinkId))
            processingMapDisableDedup.addOne(sinkId -> mutable.Set())
          processingMapDisableDedup(sinkId).addOne(sink)
        } else {
          if (!processingMap.contains(sinkId))
            processingMap.addOne(sinkId -> mutable.Set())
          processingMap(sinkId).addOne(sink)
        }
      }
      sink.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
      sink.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
    })
    processingMap
      .map(entrySet =>
        SinkProcessingModel(
          entrySet._1,
          ExporterUtility
            .convertPathElements(
              {
                if (privadoInput.disableDeDuplication)
                  entrySet._2.toList
                else
                  entrySet._2.toList
                    .distinctBy(_.code)
                    .distinctBy(_.lineNumber)
                    .distinctBy(Utilities.getFileNameForNode)
              },
              appCache = appCache,
              ruleCache = ruleCache
            )
        )
      )
      .toList ++ processingMapDisableDedup
      .map(entrySet =>
        SinkProcessingModel(
          entrySet._1,
          ExporterUtility.convertPathElements(entrySet._2.toList, appCache = appCache, ruleCache = ruleCache)
        )
      )
      .toList
  }

  /** Helper function to filter sinks
    * @param traversal
    * @return
    */
  private def filterSink(traversal: Traversal[AstNode]) = {
    traversal
      .where(_.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)))
      .whereNot(_.tag.where(_.nameExact(Constants.catLevelTwo).valueExact(Constants.leakages)))
  }

  /** Fetch all the sink node
    */
  private def getSinkList: List[AstNode] = {
    val sinks =
      cpg.identifier
        .where(filterSink)
        .l ++
        cpg.literal
          .where(filterSink)
          .l ++
        cpg.call
          .where(filterSink)
          .l ++
        cpg.templateDom
          .where(filterSink)
          .l ++ cpg.argument.isFieldIdentifier.where(filterSink).l ++ cpg.method.where(filterSink).l ++ cpg.dbNode
          .where(filterSink)
          .l
    ExporterUtility.filterNodeBasedOnRepoItemTagName(sinks, repoItemTagName)
  }

  private def convertSinkList(sinks: List[List[Tag]]) = {
    def convertSink(sinkId: String) = {
      ruleCache.getRuleInfo(sinkId) match {
        case Some(rule) =>
          val ruleInfoExporterModel = ExporterUtility.getRuleInfoForExporting(ruleCache, sinkId)
          val apiUrl = {
            if (rule.nodeType == NodeType.API) {
              val callUrls = cpg.call
                .where(_.tag.nameExact(Constants.id).valueExact(rule.id))
                .tag
                .nameExact(Constants.apiUrl + rule.id)
                .value
                .dedup
                .toArray
              val apiUrls =
                if (callUrls == null || callUrls.isEmpty || callUrls.headOption.contains(Constants.API)) {
                  cpg.templateDom
                    .where(_.tag.nameExact(Constants.id).valueExact(rule.id))
                    .tag
                    .nameExact(Constants.apiUrl + rule.id)
                    .value
                    .dedup
                    .toArray
                } else {
                  callUrls
                }
              if (apiUrls.nonEmpty) apiUrls else callUrls
            } else Array[String]()
          }
          // special case for S3 database details populated via S3Tagger
          if (rule.id.contains("AmazonS3")) {
            val s3DbDetails = s3DatabaseDetailsCache.getS3DatabaseDetails(rule.id).getOrElse(List())
            s3DbDetails.map(s3 => {
              SinkModel(
                rule.catLevelOne.label,
                rule.catLevelTwo,
                ruleInfoExporterModel.id,
                ruleInfoExporterModel.name,
                ruleInfoExporterModel.domains,
                apiUrl,
                s3
              )
            })
          } else {
            val databaseDetails = DatabaseDetailsCache.getDatabaseDetails(rule.id)
            Some(
              SinkModel(
                rule.catLevelOne.label,
                rule.catLevelTwo,
                ruleInfoExporterModel.id,
                ruleInfoExporterModel.name,
                ruleInfoExporterModel.domains,
                apiUrl,
                databaseDetails.getOrElse(DatabaseDetails("", "", "", "", ""))
              )
            )
          }
        case None => // not found anything, probably derived source
          None
      }
    }

    def getSinks(nodeList: List[Tag]) = {
      val node = nodeList
        .filterNot(node => InternalTag.valuesAsString.contains(node.name))
        .filter(node => node.name.equals(Constants.id) || node.name.startsWith(Constants.privadoDerived))
      node.value.toSet
    }
    sinks
      .flatMap(sink => getSinks(sink))
      .filter(_.nonEmpty)
      .toSet
      .flatMap(source => convertSink(source))
      .toList
  }

}
