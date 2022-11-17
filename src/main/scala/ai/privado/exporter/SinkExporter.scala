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

import ai.privado.cache.{DatabaseDetailsCache, RuleCache}
import ai.privado.entrypoint.ScanProcessor
import ai.privado.model.exporter.{SinkModel, SinkProcessingModel}
import ai.privado.model.{CatLevelOne, Constants, DatabaseDetails, InternalTag}
import ai.privado.semantic.Language.finder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, FieldIdentifier, Identifier, Literal, MethodParameterIn, StoredNode, Tag}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import scala.collection.mutable

class SinkExporter(cpg: Cpg) {

  lazy val sinkTagList: List[List[Tag]] = getSinkTagList
  lazy val sinkList: List[CfgNode]      = getSinkList

  /** Fetch and Convert sources to desired output
    */
  def getSinks: List[SinkModel] = {
    convertSinkList(sinkTagList)
  }

  def getProbableSinks: List[String] = {
    var blackListedGroupIds: List[String] = List(
      "<operator>.",
      "<operators>.",
      "iterator",
      "assert",
      "void.",
      "java.",
      "javax.",
      "ANY.",
      "android.",
      "androidx.",
      "okhttp3.",
      "io.grpc.",
      "io.netty.",
      "org.apache.http.",
      "org.json",
      "org.junit.",
      "org.apache.commons.",
      "org.apache.tomcat",
      "org.springframework.",
      "com.fasterxml.jackson.",
      "org.apache.hadoop.",
      "org.apache.giraph.",
      "org.javatuples.",
      "org.antlr.",
      "junit.framework.",
      "io.reactivex.",
      "com.bumptech.glide",
      "com.google.protobuf",
      "com.google.testing",
      "com.google.common",
      "<unresolvedNamespace>."
    )
    /** Get all the Methods which are tagged as SINKs  */
    val taggedSinkMethods = cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.l.map(i => i.methodFullName.split(":").head)

    /** Get all the Methods which are external  */
    val dependenciesTPs = cpg.method.external.l.map(i => i.fullName.split(":").head)

    /** Actions:
     * by excluding taggedSinkMethods
     * using blackListedGroupIds
     * transform method FullName close to groupIds
     * remove duplicates
     */
    val filteredTPs = dependenciesTPs
      .filter(str => !taggedSinkMethods.contains(str))
      .filter((str) => {
        val res = blackListedGroupIds.filter(blackListGroup => str.startsWith(blackListGroup))
        (res.length == 0)
      })
      .filter((str) => !str.endsWith(".println"))
      .map((str) => str.split("\\.").take(3).mkString("."))
      .distinct

    filteredTPs
  }

  def getProcessing: List[SinkProcessingModel] = {
    val processingMap = mutable.HashMap[String, mutable.Set[CfgNode]]()
    sinkList.foreach(source => {
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
        SinkProcessingModel(
          entrySet._1,
          ExporterUtility
            .convertPathElements({
              if (ScanProcessor.config.disableDeDuplication)
                entrySet._2.toList
              else
                entrySet._2.toList
                  .distinctBy(_.code)
                  .distinctBy(_.lineNumber)
                  .distinctBy(node => {
                    Traversal(node).head match {
                      case a @ (_: Identifier | _: Literal | _: MethodParameterIn | _: Call | _: FieldIdentifier) =>
                        a.file.name.head
                      case a => a.location.filename
                    }
                  })
            })
        )
      )
      .toList
  }

  /** Helper function to filter sinks
    * @param traversal
    * @return
    */
  private def filterSink(traversal: Traversal[CfgNode]) = {
    traversal
      .where(_.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)))
      .whereNot(_.tag.where(_.nameExact(Constants.catLevelTwo).valueExact(Constants.leakages)))
  }

  /** Fetch all the sink tag
    */
  private def getSinkTagList = {
    val sinks =
      cpg.identifier
        .where(filterSink)
        .map(item => item.tag.l)
        .l ++
        cpg.literal
          .where(filterSink)
          .map(item => item.tag.l)
          .l ++
        cpg.call
          .where(filterSink)
          .map(item => item.tag.l)
          .l ++ cpg.argument.isFieldIdentifier.where(filterSink).map(item => item.tag.l).l
    sinks
  }

  /** Fetch all the sink node
    */
  private def getSinkList: List[CfgNode] = {
    val sinks =
      cpg.identifier
        .where(filterSink)
        .l ++
        cpg.literal
          .where(filterSink)
          .l ++
        cpg.call
          .where(filterSink)
          .l ++ cpg.argument.isFieldIdentifier.where(filterSink).l
    sinks
  }

  private def convertSinkList(sinks: List[List[Tag]]) = {
    def convertSink(sinkId: String) = {
      RuleCache.getRuleInfo(sinkId) match {
        case Some(rule) =>
          val ruleInfoExporterModel = ExporterUtility.getRuleInfoForExporting(sinkId)
          val apiUrl = {
            if (rule.id.contains("API")) {
              cpg.call
                .where(_.tag.nameExact(Constants.id).value(rule.id))
                .tag
                .nameExact(Constants.apiUrl)
                .value
                .dedup
                .l
                .toArray
            } else
              Array[String]()
          }
          val databaseDetails = DatabaseDetailsCache.getDatabaseDetails(rule.id)
          Some(
            SinkModel(
              rule.catLevelOne.label,
              rule.catLevelTwo,
              ruleInfoExporterModel.id,
              ruleInfoExporterModel.name,
              ruleInfoExporterModel.domains,
              apiUrl,
              databaseDetails.getOrElse(DatabaseDetails("", "", "", ""))
            )
          )
        case None => // not found anything, probably derived source
          None
      }
    }

    def getSinks(nodeList: List[Tag]) = {
      val node = nodeList
        .filterNot(node => InternalTag.valuesAsString.contains(node.name))
        .filter(node => node.name.equals(Constants.id) || node.name.startsWith(Constants.privadoDerived))
      if (node.nonEmpty) {
        Some(node.value.toSet)
      } else
        None
    }
    sinks
      .flatMap(sink => getSinks(sink))
      .flatten
      .filter(_.nonEmpty)
      .toSet
      .flatMap(source => convertSink(source))
      .toList
  }

}
