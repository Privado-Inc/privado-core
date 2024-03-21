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
import ai.privado.model.{Constants, RuleInfo}
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
        var newRuleIdToUse = ruleInfo.id
        if (ruleInfo.id.equals(Constants.internalAPIRuleId)) addRuleTags(builder, apiNode, ruleInfo, ruleCache)
        else {
          val domain = resolveDomainFromSource(sourceNode)
          newRuleIdToUse = ruleInfo.id + "." + domain
          ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain))
          addRuleTags(builder, apiNode, ruleInfo, ruleCache, Some(newRuleIdToUse))
        }
        storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + newRuleIdToUse, getLiteralCode(sourceNode))
      })
      // Add url as 'API' for non Internal api nodes, so that at-least we show API without domains
      if (showAPI && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
        val literalPathApiNodes        = apiFlows.map(_.elements.last).toSet
        val apiNodesWithoutLiteralPath = apis.toSet.diff(literalPathApiNodes)
        apiNodesWithoutLiteralPath.foreach(apiNode => {
          addRuleTags(builder, apiNode, ruleInfo, ruleCache)
          storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + ruleInfo.id, Constants.API)
        })
      }
    } // Add url as 'API' for non Internal api nodes, for cases where there is no http literal present in source code
    else if (showAPI && filteredSourceNode.isEmpty && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
      apis.foreach(apiNode => {
        addRuleTags(builder, apiNode, ruleInfo, ruleCache)
        storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + ruleInfo.id, Constants.API)
      })
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

  def tagAPIWithDomainAndUpdateRuleCache(
    builder: DiffGraphBuilder,
    ruleInfo: RuleInfo,
    ruleCache: RuleCache,
    domain: String,
    apiNode: AstNode,
    apiUrlNode: AstNode
  ) = {
    val newRuleIdToUse = ruleInfo.id + "." + domain
    ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain))
    addRuleTags(builder, apiNode, ruleInfo, ruleCache, Some(newRuleIdToUse))
    storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + newRuleIdToUse, getLiteralCode(apiUrlNode))

  }
}
