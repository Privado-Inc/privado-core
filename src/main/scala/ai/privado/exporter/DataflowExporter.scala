/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.metric.MetricHandler
import ai.privado.model.{Constants, NodeType}
import ai.privado.semantic.Language.finder
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DataflowExporter(cpg: Cpg, dataflowsMap: Map[String, Path]) {

  val logger = LoggerFactory.getLogger(getClass)

  def getFlowByType(
    sinkSubCategory: String,
    sinkNodetypes: Set[String]
  ): mutable.Seq[mutable.LinkedHashMap[String, Json]] = {
    def processNonEmptyFlows(
      dataflowsMapByType: Map[String, Path]
    ): mutable.Seq[mutable.LinkedHashMap[String, Json]] = {
      val dataflowsMapBySourceId = mutable.HashMap[String, ListBuffer[String]]()
      dataflowsMapByType.foreach(entrySet => {
        def addToMap(sourceId: String) = {
          if (dataflowsMapBySourceId.contains(sourceId))
            dataflowsMapBySourceId(sourceId) += entrySet._1
          else
            dataflowsMapBySourceId.addOne(sourceId, ListBuffer(entrySet._1))
        }

        val source = entrySet._2.elements.head
        try {
          source.tag.nameExact(Constants.id).value.filter(!_.startsWith(Constants.privadoDerived)).foreach(addToMap)
          source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
        } catch {
          case e: Exception => logger.debug("Exception while traversing dataflow path : ", e)
        }
      })

      val dataflowOutputList = ListBuffer[mutable.LinkedHashMap[String, Json]]()

      dataflowsMapBySourceId.foreach(flow => {
        val dataflowOutput = mutable.LinkedHashMap[String, Json]()
        dataflowOutput.addOne(Constants.sourceId -> flow._1.asJson)
        dataflowOutput.addOne(
          Constants.sinks -> convertSinksList(
            flow._1,
            flow._2.toList,
            dataflowsMapByType,
            sinkSubCategory,
            sinkNodetypes
          )
        )
        dataflowOutputList += dataflowOutput
      })
      dataflowOutputList
    }

    val dataflowsMapByType = dataflowsMap.filter(dataflowEntrySet =>
      dataflowEntrySet._2.elements.last
        .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(sinkSubCategory))
        .nonEmpty
    )
    MetricHandler.flowCategoryData(sinkSubCategory) = dataflowsMapByType.size
    if (dataflowsMapByType.isEmpty)
      mutable.Seq[mutable.LinkedHashMap[String, Json]]()
    else
      processNonEmptyFlows(dataflowsMapByType)
  }

  private def convertSinksList(
    pathSourceId: String,
    sinkPathIds: List[String],
    dataflowsMapByType: Map[String, Path],
    dataflowSinkType: String,
    dataflowNodeTypes: Set[String]
  ) = {

    def convertSink(sinkId: String, sinkPathIds: ListBuffer[String]) = {
      var sinkOutput       = mutable.LinkedHashMap[String, Json]()
      val sinkIdAfterSplit = sinkId.split("#_#")
      sinkOutput.addOne(Constants.sinkType -> dataflowSinkType.asJson)
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
            .map(sinkPathId => convertPathsList(dataflowsMapByType(sinkPathId), sinkPathId))
            .asJson
        )
        .asJson
      sinkOutput
    }

    // sinkMap will have (sinkId -> List[String]() where value are all the paths/grouping-of-path which belong to the sinkId
    val sinkMap = mutable.HashMap[String, ListBuffer[String]]()
    distinctBySinkLineNumber(sinkPathIds, dataflowsMapByType).foreach(sinkPathId => {
      dataflowNodeTypes.foreach(dataflowNodeType => {
        // Check to filter if correct Source is consumed in Sink
        if (
          isCorrectDataSourceConsumedInSink(pathSourceId, sinkPathId, dataflowsMapByType(sinkPathId), dataflowSinkType)
        ) {
          val sinkCatLevelTwoCustomTag = dataflowsMapByType(sinkPathId).elements.last.tag
            .filter(node => node.name.equals(dataflowSinkType + dataflowNodeType))
          if (sinkCatLevelTwoCustomTag.nonEmpty) {
            var sinkId = sinkCatLevelTwoCustomTag.head.value
            val sinkAPITag = dataflowsMapByType(sinkPathId).elements.last.tag
              .filter(node => node.name.equals(Constants.apiUrl))
            if (sinkAPITag.nonEmpty) {
              sinkId += "#_#" + sinkAPITag.value.l.mkString(",")
            }
            if (!sinkMap.contains(sinkId))
              sinkMap(sinkId) = ListBuffer()
            sinkMap(sinkId).append(sinkPathId)
          }
        }
      })
    })
    sinkMap.map(entrySet => convertSink(entrySet._1, entrySet._2)).asJson
  }

  private def convertPathsList(sinkFlow: Path, pathId: String) = {
    val pathOutput = mutable.LinkedHashMap[String, Json]()

    pathOutput.addOne(Constants.pathId -> pathId.asJson)
    pathOutput.addOne(Constants.path   -> ExporterUtility.convertPathElements(sinkFlow.elements).asJson)
    pathOutput
  }

  /** Check to classify if path Source Id is getting consumed in sink for derived sources
    *
    * @param pathSourceId
    * @param path
    * @return
    */
  def isCorrectDataSourceConsumedInSink(
    pathSourceId: String,
    pathId: String,
    path: Path,
    dataflowSinkType: String
  ): Boolean = {

    if (dataflowSinkType.equals("leakages")) {
      val sourceNode      = path.elements.head
      val derivedByIdTags = sourceNode.tag.name(Constants.privadoDerived + ".*").map(tg => (tg.name, tg.value)).l
      if (derivedByIdTags.nonEmpty) {
        val disallowedMemberNameList = ListBuffer[String]()
        val sinkCode                 = path.elements.last.code
        val sinkArguments            = getArgumentListByRegex(sinkCode)
        val isCorrectFlowFromExemptedMembers = derivedByIdTags
          .filter(derivedByTag => !derivedByTag._2.equals(pathSourceId))
          .map(derivedByTag => {
            getMemberNameForForDerivedSourceNode(derivedByTag, sourceNode) match {
              case Some(memberName) =>
                disallowedMemberNameList.append(memberName)
                !isArgumentMatchingMemberName(sinkArguments, memberName)
              case None => true
            }
          })
          .foldLeft(true)((a, b) => a && b)

        if (!isCorrectFlowFromExemptedMembers) {
          var allowedMemberName = ""
          val isCorrectFlowFromSourceMember = derivedByIdTags
            .filter(derivedByTag => derivedByTag._2.equals(pathSourceId))
            .map(derivedByIdTag => {
              getMemberNameForForDerivedSourceNode(derivedByIdTag, sourceNode) match {
                case Some(memberName) =>
                  allowedMemberName = memberName
                  isArgumentMatchingMemberName(sinkArguments, memberName)
                case None => false
              }
            })
            .foldLeft(true)((a, b) => a && b)
          if (isCorrectFlowFromSourceMember) {
            logger.debug(
              s"Flow Removal : Flow removal overturned by correct SourceMember presence " +
                s"\nallowedMemberName : $allowedMemberName " +
                s"\npathSourceId : $pathSourceId " +
                s"\npathId : $pathId " +
                s"\nlogStatement : $sinkCode"
            )
            true
          } else {
            logger.debug(
              s"Flow Removal : Flow marked incorrect due to presence of other " +
                s"\ndisallowedMembers : $disallowedMemberNameList" +
                s"\nallowedMemberName : $allowedMemberName " +
                s"\npathSourceId : $pathSourceId " +
                s"\npathId : $pathId " +
                s"\nlogStatement : $sinkCode"
            )
            false
          }
        } else // Flow is correct as argument doesn't match the exempted members
          true
      } else // Flow doesn't start with a derived Source
        true
    } else // other than Leakage flow
      true
  }

  /** Fetch the member name for the given soureNode
    *
    * @param derivedByTag
    *   tag (name, value) of privadoDerived tag for derived objects
    * @param sourceNode
    * @return
    */
  private def getMemberNameForForDerivedSourceNode(derivedByTag: (String, String), sourceNode: CfgNode) = {
    val tagName  = derivedByTag._1
    val tagValue = derivedByTag._2
    sourceNode.tag.nameExact(tagValue + Constants.underScore + tagName).value.l.headOption
  }

  /** Apply regex to fetch the arguments in the given source code of a sink
    * @param sinkCode
    * @return
    */
  private def getArgumentListByRegex(sinkCode: String): List[String] = {
    sinkCode.split("[(,+]").map(argument => argument.strip().replaceAll("\".*\"", "")).filter(_.nonEmpty).toList
  }

  /** Verify if Member name matches any of the argument name by regex
    *
    * @param sinkArgument
    * @param memberName
    * @return
    */
  private def isArgumentMatchingMemberName(sinkArgument: List[String], memberName: String): Boolean = {
    sinkArgument.map(argument => argument.matches("(?i).*" + memberName + ".*")).foldLeft(false)((a, b) => a || b)
  }

  /** Distinct by operation on sink -> fileName + lineNumber to return a unique path
    * @param sinkPathIds
    * @param dataflowsMapByType
    * @return
    */
  def distinctBySinkLineNumber(sinkPathIds: List[String], dataflowsMapByType: Map[String, Path]): List[String] = {
    val uniqueSinkMap = mutable.HashMap[String, String]() // Stores fileLineNo --> pathId
    sinkPathIds.foreach(sinkPathId => {
      val sinkNodeWithLocation = dataflowsMapByType(sinkPathId).elements.last.location
      val fileLineNo           = sinkNodeWithLocation.lineNumber.getOrElse(0).toString + sinkNodeWithLocation.filename
      val flowSize             = dataflowsMapByType(sinkPathId).elements.size

      if (uniqueSinkMap.contains(fileLineNo)) {
        if (flowSize > dataflowsMapByType(uniqueSinkMap(fileLineNo)).elements.size)
          uniqueSinkMap(fileLineNo) = sinkPathId
      } else
        uniqueSinkMap(fileLineNo) = sinkPathId
    })
    uniqueSinkMap.values.toList
  }

}
