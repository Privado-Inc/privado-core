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

package ai.privado.tagger.utility

import ai.privado.cache.RuleCache
import ai.privado.dataflow.DuplicateFlowProcessor
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.language.NodeToProperty
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.model.{Constants, FilterProperty, RuleInfo}
import ai.privado.utility.Utilities.{
  addRuleTags,
  getDomainFromString,
  getFileNameForNode,
  isFileProcessable,
  storeForTag
}
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode, JavaProperty, Member}
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.languageEngine.java.language._
import io.shiftleft.semanticcpg.language._

object APITaggerUtility {

  // for cases where services defined as https://exampleService
  val SERVICE_URL_REGEX_PATTERN = ".*(http|https):\\/\\/[a-zA-Z0-9_-]+$"

  def getLiteralCode(element: AstNode): String = {
    val literalCode = element match {
      case member: Member             => member.name
      case propertyNode: JavaProperty => propertyNode.value
      case _                          => element.code.split(" ").last
    }

    element.originalPropertyValue.getOrElse(literalCode)
  }

  def sinkTagger(
    cpg: Cpg,
    apiInternalSinkPattern: List[AstNode],
    apis: List[CfgNode],
    builder: BatchedUpdate.DiffGraphBuilder,
    ruleInfo: RuleInfo,
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    showAPI: Boolean = true
  )(implicit engineContext: EngineContext): Unit = {
    val filteredSourceNode =
      apiInternalSinkPattern.filter(node => isFileProcessable(getFileNameForNode(node), ruleCache))
    if (apis.nonEmpty && filteredSourceNode.nonEmpty) {
      val apiFlows = {
        val flows = apis.reachableByFlows(filteredSourceNode)(engineContext).toList
        if (privadoInput.disableDeDuplication)
          flows
        else
          DuplicateFlowProcessor.getUniquePathsAfterDedup(flows)
      }
      apiFlows.foreach(flow => {
        val sourceNode = flow.elements.head
        val apiNode    = flow.elements.last
        // Tag API's when we find a dataflow to them
        if ruleInfo.id.equals(Constants.internalAPIRuleId) then
          tagAPINode(builder, ruleCache, ruleInfo, apiNode, getLiteralCode(sourceNode))
        else
          tagThirdPartyAPIWithDomainAndUpdateRuleCache(
            builder,
            cpg,
            ruleCache,
            resolveDomainFromSource(sourceNode),
            apiNode,
            sourceNode
          )
      })
      // Add url as 'API' for non Internal api nodes, so that at-least we show API without domains
      if (showAPI && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
        val literalPathApiNodes        = apiFlows.map(_.elements.last).toSet
        val apiNodesWithoutLiteralPath = apis.toSet.diff(literalPathApiNodes)
        apiNodesWithoutLiteralPath.foreach(tagAPINode(builder, ruleCache, ruleInfo, _, Constants.API))
      }
    } // Add url as 'API' for non Internal api nodes, for cases where there is no http literal present in source code
    else if (showAPI && filteredSourceNode.isEmpty && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
      apis.foreach(tagAPINode(builder, ruleCache, ruleInfo, _, Constants.API))
    }
  }

  def cleanUrl(urlValue: String): String = {
    urlValue.stripPrefix("\"").stripSuffix("\"")
  }

  def resolveDomainFromSource(sourceNode: AstNode): String = {
    val sourceDomain = sourceNode.originalPropertyValue.getOrElse(getLiteralCode(sourceNode))
    if (sourceDomain.matches(SERVICE_URL_REGEX_PATTERN)) {
      sourceDomain.split("//").last
    } else {
      getDomainFromString(sourceDomain)
    }
  }

  /** Tag API or internal api nodes, Don't use this for tagging Third Party API's
    * @param builder
    * @param ruleCache
    * @param apiNode
    * @param apiUrl
    * @return
    */
  private def tagAPINode(
    builder: DiffGraphBuilder,
    ruleCache: RuleCache,
    ruleInfo: RuleInfo,
    apiNode: AstNode,
    apiUrl: String
  ) = {
    addRuleTags(builder, apiNode, ruleInfo, ruleCache)
    storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + ruleInfo.id, apiUrl)
  }

  def tagThirdPartyAPIWithDomainAndUpdateRuleCache(
    builder: DiffGraphBuilder,
    cpg: Cpg,
    ruleCache: RuleCache,
    domain: String,
    apiNode: AstNode,
    apiUrlNode: Any
  ): Unit = {
    // TODO Optimise this by adding a cache mechanism
    val flatMapList = ruleCache.getRule.inferences
      .filter(_.catLevelTwo.equals(Constants.apiEndpoint))
      .filter(_.domains.nonEmpty)
      .flatMap { ruleInfo =>
        ruleInfo.filterProperty match {
          case FilterProperty.ENDPOINT_DOMAIN_WITH_LITERAL if domain.matches(ruleInfo.combinedRulePattern) =>
            Some(ruleInfo.domains.head)
          case FilterProperty.ENDPOINT_DOMAIN_WITH_PROPERTY_NAME
              if domain.matches(ruleInfo.combinedRulePattern) && cpg.property.name(ruleInfo.domains.head).nonEmpty =>
            Some(cpg.property.name(ruleInfo.domains.head).value.head)
          case _ => None
        }
      }
    ruleCache.getRule.inferences
      .filter(_.catLevelTwo.equals(Constants.apiEndpoint))
      .filter(_.domains.nonEmpty)
      .flatMap { ruleInfo =>
        ruleInfo.filterProperty match {
          case FilterProperty.ENDPOINT_DOMAIN_WITH_LITERAL if domain.matches(ruleInfo.combinedRulePattern) =>
            Some(ruleInfo.domains.head)
          case FilterProperty.ENDPOINT_DOMAIN_WITH_PROPERTY_NAME
              if domain.matches(ruleInfo.combinedRulePattern) && cpg.property.name(ruleInfo.domains.head).nonEmpty =>
            Some(cpg.property.name(ruleInfo.domains.head).value.head)
          case _ => None
        }
      }
      .headOption match
      case Some(inferredDomain) =>
        addThirdPartyRuleAndTagAPI(builder, ruleCache, inferredDomain, apiNode, inferredDomain)
      case None =>
        apiUrlNode match {
          case x: String  => addThirdPartyRuleAndTagAPI(builder, ruleCache, domain, apiNode, x)
          case x: AstNode => addThirdPartyRuleAndTagAPI(builder, ruleCache, domain, apiNode, getLiteralCode(x))
          case _          =>
        }
  }

  /** Generates a new third party rule, updates ruleCache, and tag the apiSink with this generated rule
    * @param builder
    * @param ruleCache
    * @param domain
    * @param apiNode
    * @param apiUrl
    * @return
    */
  private def addThirdPartyRuleAndTagAPI(
    builder: DiffGraphBuilder,
    ruleCache: RuleCache,
    domain: String,
    apiNode: AstNode,
    apiUrl: String
  ) = {
    ruleCache.getRuleInfo(Constants.thirdPartiesAPIRuleId) match
      case Some(thirdPartyAPIRuleInfo) =>
        val newRuleId = ruleCache.addThirdPartyRuleInfo(thirdPartyAPIRuleInfo, domain)
        addRuleTags(builder, apiNode, thirdPartyAPIRuleInfo, ruleCache, Some(newRuleId))
        storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + newRuleId, apiUrl)
      case None => // Third party rule doesn't exist, which is ideally not possible
  }
}
