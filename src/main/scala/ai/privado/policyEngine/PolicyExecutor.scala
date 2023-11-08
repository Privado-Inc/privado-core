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

package ai.privado.policyEngine

import ai.privado.cache.{DataFlowCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.getSourceNode
import ai.privado.model.exporter.{ViolationDataFlowModel, ViolationProcessingModel}
import ai.privado.exporter.SourceExporter
import ai.privado.model.{Constants, PolicyAction, PolicyOrThreat}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Tag}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class PolicyExecutor(
  cpg: Cpg,
  dataFlowCache: DataFlowCache,
  repoName: String,
  ruleCache: RuleCache,
  privadoInput: PrivadoInput
) {

  private val logger = LoggerFactory.getLogger(getClass)

  val ALL_MATCH_REGEX                             = "**"
  val actionMap: Map[PolicyAction.Value, Boolean] = Map(PolicyAction.ALLOW -> false, PolicyAction.DENY -> true)
  lazy val policies: List[PolicyOrThreat] = ruleCache.getAllPolicy.filter(policy => filterByRepoName(policy, repoName))

  // Map to contain sourceId -> List(pathIds)
  lazy val dataflowSourceIdMap: Map[String, List[String]] = getDataflowBySourceIdMapping

  // Map to contain sinkId -> List(pathIds)
  lazy val dataflowSinkIdMap: Map[String, List[String]] = getDataflowBySinkIdMapping

  val sourceExporter = new SourceExporter(cpg, ruleCache, privadoInput)

  lazy val sourceExporterModel = sourceExporter.getSources

  def getSourcesMatchingRegexForProcessing(policy: PolicyOrThreat) = {
    var matchingSourceModels = policy.dataFlow.sources
      .flatMap(policySourceRegex => {
        if (policySourceRegex.equals(ALL_MATCH_REGEX)) {
          sourceExporterModel
        } else {
          sourceExporterModel.flatMap(sourceModelItem => {
            if (sourceModelItem.id.matches(policySourceRegex))
              Some(sourceModelItem)
            else
              None
          })
        }
      })
      .toSet
    if (policy.dataFlow.sourceFilters.sensitivity.nonEmpty)
      matchingSourceModels =
        matchingSourceModels.filter(_.sensitivity.equals(policy.dataFlow.sourceFilters.sensitivity))

    if (policy.dataFlow.sourceFilters.isSensitive.isDefined)
      matchingSourceModels = matchingSourceModels.filter(_.isSensitive == policy.dataFlow.sourceFilters.isSensitive.get)

    matchingSourceModels.map(_.id)
  }

  /** Processes Processing style of policy and returns affected SourceIds
    */
  def getProcessingViolations: Map[String, List[(String, CfgNode)]] = {
    val processingTypePolicy = policies.filter(policy => policy.dataFlow.sinks.isEmpty)
    val processingResult = processingTypePolicy
      .map(policy =>
        (
          policy.id,
          getSourcesMatchingRegexForProcessing(policy).toList.flatMap(sourceId => getSourceNode(this.cpg, sourceId))
        )
      )
      .toMap
    processingResult
  }

  /** Processes Dataflow style of policy and returns affected SourceIds
    */
  def getDataflowViolations: Map[String, mutable.HashSet[ViolationDataFlowModel]] = {
    val dataflowResult = policies
      .map(policy => (policy.id, getViolatingFlowsForPolicy(policy)))
      .toMap
    dataflowResult
  }

  def getViolatingFlowsForPolicy(policy: PolicyOrThreat): mutable.HashSet[ViolationDataFlowModel] = {
    val violatingFlowList = mutable.HashSet[ViolationDataFlowModel]()
    val sourceMatchingIds = getSourcesMatchingRegex(policy)
    val sinksMatchingIds  = getSinksMatchingRegex(policy)
    sourceMatchingIds.foreach(sourceId => {
      sinksMatchingIds.foreach(sinkId => {
        val intersectingPathIds = dataflowSourceIdMap(sourceId).intersect(dataflowSinkIdMap(sinkId)).dedup.toList
        if (intersectingPathIds.nonEmpty) {
          violatingFlowList.add(ViolationDataFlowModel(sourceId, sinkId, intersectingPathIds))
        }
      })
    })
    violatingFlowList
  }

  def getViolatingOccurrencesForPolicy(policy: PolicyOrThreat): mutable.HashSet[ViolationProcessingModel] = {
    val violatingFlowList = mutable.HashSet[ViolationProcessingModel]()
    getSourcesMatchingRegex(policy).foreach(sourceId => {
      val sourceNode = getSourceNode(this.cpg, sourceId)
      if (!sourceNode.isEmpty) {
        sourceNode.foreach((sourceNode) => {
          violatingFlowList.add(
            ViolationProcessingModel(sourceNode._1, ExporterUtility.convertIndividualPathElement(sourceNode._2), None)
          )
        })
      }
    })
    violatingFlowList
  }

  /** Filters outs based on Repository name
    */
  private def filterByRepoName(policy: PolicyOrThreat, repoName: String): Boolean = {
    actionMap.get(policy.action) match {
      case Some(value) =>
        value == policy.repositories
          .map(repoPattern => { repoPattern.equals(ALL_MATCH_REGEX) || repoName.matches(repoPattern) })
          .reduce((a, b) => a || b)
      case None => true
    }
  }

  private def getDataflowBySourceIdMapping = {
    dataFlowCache.getDataflow.groupBy(_.sourceId).map(entrySet => (entrySet._1, entrySet._2.map(_.pathId)))
  }

  private def getDataflowBySinkIdMapping = {
    dataFlowCache.getDataflow.groupBy(_.sinkId).map(entrySet => (entrySet._1, entrySet._2.map(_.pathId)))
  }

  private def getSourcesMatchingRegex(policy: PolicyOrThreat): Set[String] = {
    val sourceFilters = policy.dataFlow.sourceFilters
    var matchingSourceIds = policy.dataFlow.sources
      .flatMap(policySourceRegex => {
        if (policySourceRegex.equals(ALL_MATCH_REGEX)) {
          dataflowSourceIdMap.keys
        } else {
          dataflowSourceIdMap.flatMap(sourceIdEntry => {
            if (sourceIdEntry._1.matches(policySourceRegex))
              Some(sourceIdEntry._1)
            else
              None
          })
        }
      })
      .toSet

    if (sourceFilters.sensitivity.nonEmpty)
      matchingSourceIds =
        matchingSourceIds.filter(ruleCache.getRuleInfo(_).get.sensitivity.equals(sourceFilters.sensitivity))

    if (sourceFilters.isSensitive.isDefined)
      matchingSourceIds =
        matchingSourceIds.filter(ruleCache.getRuleInfo(_).get.isSensitive == sourceFilters.isSensitive.get)

    if (sourceFilters.name.nonEmpty)
      val namePattern: Option[Regex] = sourceFilters.name match {
        case "" => None
        case name => Some(s"(?i)\\Q$name\\E".r)
      }
      matchingSourceIds = matchingSourceIds.filter { sinkId =>
        val ruleInfo = ruleCache.getRuleInfo(sinkId)
        namePattern.exists(pattern =>
          ruleInfo.exists(info => pattern.findFirstIn(info.name.toLowerCase).nonEmpty)
        )
      }

    matchingSourceIds
  }

  private def getSinksMatchingRegex(policy: PolicyOrThreat) = {
    val sinkFilters = policy.dataFlow.sinkFilters
    var matchingSinkIds = policy.dataFlow.sinks
      .flatMap(policySinkRegex => {
        if (policySinkRegex.equals(ALL_MATCH_REGEX)) {
          dataflowSinkIdMap.keys
        } else {
          dataflowSinkIdMap.flatMap(sinkIdEntry => {
            if (sinkIdEntry._1.matches(policySinkRegex))
              // == actionMap.getOrElse(policy.action, true))
              Some(sinkIdEntry._1)
            else
              None
          })
        }
      })
      .toSet
    if (sinkFilters.sinkType.nonEmpty)
      matchingSinkIds =
        matchingSinkIds.filter(ruleCache.getRuleInfo(_).get.id.toLowerCase.contains(sinkFilters.sinkType.toLowerCase))

    if (sinkFilters.domains.nonEmpty) {
      matchingSinkIds = matchingSinkIds.filter { sinkId =>
        val ruleInfo = ruleCache.getRuleInfo(sinkId)
        ruleInfo.exists(info => info.domains.intersect(sinkFilters.domains).nonEmpty)
      }
    }

    if (sinkFilters.name.nonEmpty)
      val namePattern: Option[Regex] = sinkFilters.name match {
        case "" => None
        case name => Some(s"(?i)\\Q$name\\E".r)
      }
      matchingSinkIds = matchingSinkIds.filter { sinkId =>
        val ruleInfo = ruleCache.getRuleInfo(sinkId)
        namePattern.exists(pattern =>
          ruleInfo.exists(info => pattern.findFirstIn(info.name.toLowerCase).nonEmpty)
        )
      }

    matchingSinkIds
  }

}
