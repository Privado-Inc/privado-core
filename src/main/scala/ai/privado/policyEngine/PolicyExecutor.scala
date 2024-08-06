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

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.ExporterUtility
import ai.privado.threatEngine.ThreatUtility.getSourceNode
import ai.privado.model.exporter.{
  CollectionModel,
  CollectionOccurrenceDetailModel,
  CollectionOccurrenceModel,
  DataFlowSubCategoryPathExcerptModel,
  ViolationDataFlowModel,
  ViolationProcessingModel
}
import ai.privado.exporter.SourceExporter
import ai.privado.model.{Constants, DataFlowPathModel, PolicyAction, PolicyOrThreat}
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
  dataFlowModel: List[DataFlowPathModel],
  repoName: String,
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  collections: List[CollectionModel] = List[CollectionModel](),
  appCache: AppCache
) {

  private val logger = LoggerFactory.getLogger(getClass)

  val ALL_MATCH_REGEX                             = "**"
  val actionMap: Map[PolicyAction.Value, Boolean] = Map(PolicyAction.ALLOW -> false, PolicyAction.DENY -> true)
  lazy val policies: List[PolicyOrThreat] = ruleCache.getAllPolicy.filter(policy => filterByRepoName(policy, repoName))

  // Map to contain sourceId -> List(pathIds)
  lazy val dataflowSourceIdMap: Map[String, List[String]] = getDataflowBySourceIdMapping

  // Map to contain sinkId -> List(pathIds)
  lazy val dataflowSinkIdMap: Map[String, List[String]] = getDataflowBySinkIdMapping

  val sourceExporter = new SourceExporter(cpg, ruleCache, privadoInput, appCache = appCache)

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
    val processingTypePolicy =
      policies.filter(policy => policy.dataFlow.sinks.isEmpty && policy.dataFlow.collectionFilters.equals(("", "")))
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

  def getCollectionViolations: Map[String, List[ViolationProcessingModel]] = {
    val processingTypePolicy = policies.filter(policy =>
      policy.dataFlow.sinks.isEmpty && policy.dataFlow.collectionFilters.collectionType.nonEmpty
    )
    val collectionResult = processingTypePolicy
      .map(policy => (policy.id, getCollectionFlowsForPolicy(policy).toList))
      .toMap
    collectionResult
  }

  /** Processes Dataflow style of policy and returns affected SourceIds
    */
  def getDataflowViolations: Map[String, mutable.HashSet[ViolationDataFlowModel]] = {
    val dataflowResult = policies
      .map(policy => (policy.id, getViolatingFlowsForPolicy(policy)))
      .toMap
    dataflowResult
  }

  def getCollectionFlowsForPolicy(policy: PolicyOrThreat): mutable.HashSet[ViolationProcessingModel] = {
    val violatingProcessingList        = mutable.HashSet[ViolationProcessingModel]()
    val sourceMatchingIds              = getSourcesMatchingRegex(policy)
    val isCollectionOfFormType         = policy.dataFlow.collectionFilters.collectionType.equals("form")
    val collectionEndpoint             = policy.dataFlow.collectionFilters.endPoint
    val endpointPattern: Option[Regex] = Some(collectionEndpoint.r)
    val FORM_TYPE_ID                   = "Collections.Webforms"
    val filteredCollections = collections.filter { collectionModel =>
      if (isCollectionOfFormType) collectionModel.collectionId == FORM_TYPE_ID
      else !collectionModel.collectionId.equals(FORM_TYPE_ID)
    }

    def createViolationProcessingModel(sourceId: String, oc: CollectionOccurrenceModel): ViolationProcessingModel =
      ViolationProcessingModel(
        sourceId = sourceId,
        occurrence = Some(
          DataFlowSubCategoryPathExcerptModel(
            sample = oc.sample,
            lineNumber = oc.lineNumber,
            columnNumber = oc.columnNumber,
            fileName = oc.fileName,
            excerpt = oc.excerpt
          )
        ),
        detail = Some(oc.endPoint)
      )

    def checkAndAddViolationModel(sourceId: String, oc: CollectionOccurrenceModel): Unit = {
      val endpoint                 = oc.endPoint
      val violationProcessingModel = createViolationProcessingModel(sourceId, oc)
      if (collectionEndpoint.isEmpty || endpointPattern.exists(pattern => pattern.findFirstIn(endpoint).nonEmpty))
        violatingProcessingList.add(violationProcessingModel)
    }

    filteredCollections.foreach { cM =>
      cM.collections.foreach { collection =>
        val sourceId = collection.sourceId
        if (sourceMatchingIds.isEmpty) {
          collection.occurrences.foreach { oc =>
            checkAndAddViolationModel(sourceId, oc)
          }
        } else if (sourceMatchingIds.contains(sourceId)) {
          collection.occurrences.foreach { oc =>
            checkAndAddViolationModel(sourceId, oc)
          }
        }
      }
    }
    violatingProcessingList
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

  def getViolatingOccurrencesForPolicy(
    policy: PolicyOrThreat,
    appCache: AppCache
  ): mutable.HashSet[ViolationProcessingModel] = {
    val violatingFlowList = mutable.HashSet[ViolationProcessingModel]()
    getSourcesMatchingRegex(policy).foreach(sourceId => {
      val sourceNode = getSourceNode(this.cpg, sourceId)
      if (!sourceNode.isEmpty) {
        sourceNode.foreach((sourceNode) => {
          violatingFlowList.add(
            ViolationProcessingModel(
              sourceNode._1,
              ExporterUtility.convertIndividualPathElement(sourceNode._2, appCache = appCache, ruleCache = ruleCache),
              None
            )
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
    dataFlowModel.groupBy(_.sourceId).map(entrySet => (entrySet._1, entrySet._2.map(_.pathId)))
  }

  private def getDataflowBySinkIdMapping = {
    dataFlowModel.groupBy(_.sinkId).map(entrySet => (entrySet._1, entrySet._2.map(_.pathId)))
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
      val namePattern: Option[Regex] = Some(sourceFilters.name.r)
      matchingSourceIds = matchingSourceIds.filter { sinkId =>
        val ruleInfo = ruleCache.getRuleInfo(sinkId)
        namePattern.exists(pattern => ruleInfo.exists(info => pattern.findFirstIn(info.name).nonEmpty))
      }
    if (sourceFilters.allowedSourceFilters.sources.nonEmpty) {
      matchingSourceIds = matchingSourceIds.filter { sourceId =>
        sourceFilters.allowedSourceFilters.sources
          .filter(d => {
            val sourcePattern: Regex = d.r
            sourcePattern.findFirstIn(sourceId).nonEmpty
          })
          .isEmpty
      }
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
        // ----------------Covering Cases:
        // Sinks.ThirdParties.API.mediaconvert.awsRegion.amazonaws.com
        // Sinks.ThirdParties.API.axios.com
        // ThirdParties.SDK.Sendgrid
        // ----------------Not covered:
        // Sinks.API.InternalAPI
        // Sinks.ThirdParties.API
        if (sinkId.contains(f"${Constants.thirdPartiesAPIRuleId}.")) {
          sinkFilters.domains
            .filter(d => {
              val domainPattern: Regex = d.r
              domainPattern.findFirstIn(sinkId).nonEmpty
            })
            .nonEmpty
        } else {
          ruleInfo.exists(info => info.domains.intersect(sinkFilters.domains).nonEmpty)
        }
      }
    }

    if (sinkFilters.allowedSinkFilters.domains.nonEmpty) {
      matchingSinkIds = matchingSinkIds.filter { sinkId =>
        val ruleInfo = ruleCache.getRuleInfo(sinkId)
        // ----------------Covering Cases:
        // Sinks.ThirdParties.API.mediaconvert.awsRegion.amazonaws.com
        // Sinks.ThirdParties.API.axios.com
        // ThirdParties.SDK.Sendgrid
        // ----------------Not covered:
        // Sinks.API.InternalAPI
        // Sinks.ThirdParties.API
        if (sinkId.contains(f"${Constants.thirdPartiesAPIRuleId}.")) {
          sinkFilters.allowedSinkFilters.domains
            .filter(d => {
              val domainPattern: Regex = d.r
              domainPattern.findFirstIn(sinkId).nonEmpty
            })
            .isEmpty
        } else {
          ruleInfo.exists(info => info.domains.intersect(sinkFilters.allowedSinkFilters.domains).isEmpty)
        }
      }
    }

    if (sinkFilters.name.nonEmpty)
      val namePattern: Option[Regex] = Some(sinkFilters.name.r)
      matchingSinkIds = matchingSinkIds.filter { sinkId =>
        val ruleInfo = ruleCache.getRuleInfo(sinkId)
        namePattern.exists(pattern => ruleInfo.exists(info => pattern.findFirstIn(info.name).nonEmpty))
      }

    matchingSinkIds
  }

}
