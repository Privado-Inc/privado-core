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

package ai.privado.dataflow

import ai.privado.cache.{DataFlowCache, RuleCache}
import ai.privado.entrypoint.ScanProcessor
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, Constants, DataFlowPathModel, NodeType}
import ai.privado.semantic.Language.finder
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, StoredNode}
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class Dataflow(cpg: Cpg) {

  private val logger                        = LoggerFactory.getLogger(getClass)
  implicit val engineContext: EngineContext = EngineContext(Utilities.getSemantics(cpg), EngineConfig(4))

  val falsePositiveSources = List[String](
    "Data.Sensitive.OnlineIdentifiers.Cookies",
    "Data.Sensitive.OnlineIdentifiers.IPAddress",
    "Data.Sensitive.PersonalCharacteristics.Signature",
    "Data.Sensitive.BiometricData.FingerprintScans"
  )

  /** Compute the flow of data from tagged Sources to Sinks
    * @return
    *   \- Map of PathId -> Path corresponding to source to sink path
    */
  def dataflow: Map[String, Path] = {

    logger.info("Generating dataflow")
    val sources = getSources
    val sinks   = getSinks

    if (sources.isEmpty || sinks.isEmpty)
      Map[String, Path]()
    else {
      val dataflowPaths = sinks.reachableByFlows(sources).l
      // Stores key -> PathID, value -> Path
      var dataflowMapByPathId = Map[String, Path]()
      if (ScanProcessor.config.disableDeDuplication) {
        dataflowMapByPathId = dataflowPaths
          .flatMap(dataflow => {
            DuplicateFlowProcessor.calculatePathId(dataflow) match {
              case Success(pathId) => Some(pathId, dataflow)
              case Failure(e) =>
                logger.debug("Exception : ", e)
                None
            }
          })
          .toMap
      } else {
        println("Deduplicating data flows...")
        dataflowMapByPathId = DuplicateFlowProcessor.process(dataflowPaths)
      }
      filterIrrelevantFlowsAndStoreInCache(dataflowMapByPathId)
      // Need to return the filtered result
      val dataflowFromCache = DataFlowCache.getDataflow
      dataflowFromCache.map(_.pathId).toSet.map((pathId: String) => (pathId, dataflowMapByPathId(pathId))).toMap
    }
  }

  private def getSources: List[CfgNode] = {
    def filterSources(traversal: Traversal[StoredNode]) = {
      traversal.tag
        .nameExact(Constants.catLevelOne)
        .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
    }
    cpg.literal
      .where(filterSources)
      .l ++ cpg.identifier
      .where(filterSources)
      .l ++ cpg.call
      .where(filterSources)
      .l

  }

  private def getSinks: List[CfgNode] = {
    cpg.call.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l
  }

  private def filterIrrelevantFlowsAndStoreInCache(dataflowMapByPathId: Map[String, Path]) = {

    val sinkSubCategories = mutable.HashMap[String, mutable.Set[String]]()
    RuleCache.getRule.sinks.foreach(sinkRule => {
      if (!sinkSubCategories.contains(sinkRule.catLevelTwo))
        sinkSubCategories.addOne(sinkRule.catLevelTwo -> mutable.Set())
      sinkSubCategories(sinkRule.catLevelTwo).add(sinkRule.nodeType.toString)
    })

    sinkSubCategories.foreach(sinkSubTypeEntry =>
      filterFlowsBySubCategoryNodeTypeAndStoreInCache(
        dataflowMapByPathId,
        sinkSubTypeEntry._1,
        sinkSubTypeEntry._2.toSet
      )
    )
  }
  private def filterFlowsBySubCategoryNodeTypeAndStoreInCache(
    dataflowMapByPathId: Map[String, Path],
    sinkSubCategory: String,
    sinkNodetypes: Set[String]
  ) = {
    val dataflowsMapByType = dataflowMapByPathId.filter(dataflowEntrySet =>
      dataflowEntrySet._2.elements.last
        .where(_.tag.nameExact(Constants.catLevelTwo).valueExact(sinkSubCategory))
        .nonEmpty
    )
    // Metric for sinkSubCategory size
    MetricHandler.flowCategoryData(sinkSubCategory) = dataflowsMapByType.size

    // Store sourceId -> List[PathIds] Paths which have sourceId as the source
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
        case e: Exception => logger.debug("Exception while fetching sourceId in dataflow : ", e)
      }
    })

    dataflowsMapBySourceId.foreach(flow => {
      filterSinkListAndStoreInCache(flow._1, flow._2.toList, dataflowsMapByType, sinkSubCategory, sinkNodetypes)
    })
  }

  private def filterSinkListAndStoreInCache(
    pathSourceId: String,
    sinkPathIds: List[String],
    dataflowsMapByType: Map[String, Path],
    dataflowSinkType: String,
    dataflowNodeTypes: Set[String]
  ) = {

    def addToCache(sinkPathId: String, dataflowNodeType: String) = {
      val sinkCatLevelTwoCustomTag = dataflowsMapByType(sinkPathId).elements.last.tag
        .filter(node => node.name.equals(dataflowSinkType + dataflowNodeType))
      if (sinkCatLevelTwoCustomTag.nonEmpty) {
        val sinkId = sinkCatLevelTwoCustomTag.head.value
        DataFlowCache.setDataflow(
          DataFlowPathModel(pathSourceId, sinkId, dataflowSinkType, dataflowNodeType, sinkPathId)
        )
      }
    }

    if (ScanProcessor.config.disableDeDuplication) {
      sinkPathIds.foreach(sinkPathId => {
        dataflowNodeTypes.foreach(dataflowNodeType => {
          // Add this to Cache
          addToCache(sinkPathId, dataflowNodeType)
        })
      })
    } else {
      distinctBySinkLineNumber(sinkPathIds, dataflowsMapByType).foreach(sinkPathId => {
        dataflowNodeTypes.foreach(dataflowNodeType => {
          // Check to filter if correct Source is consumed in Sink
          if (
            isCorrectDataSourceConsumedInSink(
              pathSourceId,
              sinkPathId,
              dataflowsMapByType(sinkPathId),
              dataflowSinkType,
              dataflowNodeType
            )
          ) {
            // Add this to Cache
            addToCache(sinkPathId, dataflowNodeType)
          }
        })
      })
    }
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
    dataflowSinkType: String,
    dataflowNodeType: String
  ): Boolean = {

    var isCorrect = true // By default we set it to true, mark it false if matches some condition

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
            getMemberNameForDerivedSourceNode(derivedByTag, sourceNode) match {
              case Some((memberName, memberRuleId)) =>
                disallowedMemberNameList.append(memberName)
                !(isArgumentMatchingMemberName(sinkArguments, memberName) || isArgumentMatchingMemberPattern(
                  sinkArguments,
                  memberRuleId
                ))
              case None => true
            }
          })
          .foldLeft(true)((a, b) => a && b)

        if (!isCorrectFlowFromExemptedMembers) {
          var allowedMemberName = ""
          val isCorrectFlowFromSourceMember = derivedByIdTags
            .filter(derivedByTag => derivedByTag._2.equals(pathSourceId))
            .map(derivedByIdTag => {
              getMemberNameForDerivedSourceNode(derivedByIdTag, sourceNode) match {
                case Some((memberName, memberRuleId)) =>
                  allowedMemberName = memberName
                  isArgumentMatchingMemberName(sinkArguments, memberName) || isArgumentMatchingMemberPattern(
                    sinkArguments,
                    memberRuleId
                  )
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
          } else {
            logger.debug(
              s"Flow Removal : Flow marked incorrect due to presence of other member" +
                s"\ndisallowedMembers : $disallowedMemberNameList" +
                s"\nallowedMemberName : $allowedMemberName " +
                s"\npathSourceId : $pathSourceId " +
                s"\npathId : $pathId " +
                s"\nlogStatement : $sinkCode"
            )
            isCorrect = false
          }
        }
      }
    } else if (dataflowSinkType.equals("third_parties") || dataflowNodeType.equals(NodeType.API.toString)) {
      // To explicity remove Sources which result in FP
      if (falsePositiveSources.contains(pathSourceId))
        isCorrect = false
    }
    isCorrect
  }

  /** Fetch the member name for the given soureNode
    *
    * @param derivedByTag
    *   tag (name, value) of privadoDerived tag for derived objects
    * @param sourceNode
    * @return
    */
  private def getMemberNameForDerivedSourceNode(derivedByTag: (String, String), sourceNode: CfgNode) = {
    val tagName  = derivedByTag._1
    val tagValue = derivedByTag._2
    sourceNode.tag.nameExact(tagValue + Constants.underScore + tagName).value.l.headOption match {
      case Some(memberName) => Some((memberName, tagValue))
      case None             => None
    }
  }

  /** Apply regex to fetch the arguments in the given source code of a sink
    * @param sinkCode
    * @return
    */
  private def getArgumentListByRegex(sinkCode: String): List[String] = {
    sinkCode.split("[(,+]").map(argument => argument.strip().replaceAll("\".*\"", "")).filter(_.nonEmpty).toList
  }

  /** Verify if pattern of matched Member, matches any of the argument name by regex
    *
    * @param sinkArgument
    * @param memberRuleId
    * @return
    */
  private def isArgumentMatchingMemberPattern(sinkArgument: List[String], memberRuleId: String): Boolean = {
    Try(
      sinkArgument
        .map(argument => argument.matches(RuleCache.getRuleInfo(memberRuleId).get.patterns.head))
        .foldLeft(false)((a, b) => a || b)
    ) match {
      case Success(result) => result
      case Failure(_)      => false
    }
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
}
