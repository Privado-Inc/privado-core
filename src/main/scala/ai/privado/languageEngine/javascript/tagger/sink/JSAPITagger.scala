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

package ai.privado.languageEngine.javascript.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.dataflow.DuplicateFlowProcessor
import ai.privado.entrypoint.ScanProcessor
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.sink.APITagger

import scala.collection.mutable.ListBuffer
import ai.privado.languageEngine.java.language.{NodeStarters, NodeToProperty, StepsForProperty}
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.utility.Utilities.{addRuleTags, getDomainFromTemplates, storeForTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode}
import io.joern.dataflowengineoss.DefaultSemantics
import ai.privado.utility.Utilities.{
  addRuleTags,
  getDomainFromString,
  getFileNameForNode,
  isFileProcessable,
  storeForTag
}
import io.joern.dataflowengineoss.language.toExtendedCfgNode
import overflowdb.BatchedUpdate

// class GraphqlAPITagger
class JSAPITagger(cpg: Cpg, ruleCache: RuleCache) extends APITagger(cpg, ruleCache) {

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    super.runOnPart(builder, ruleInfo)

    // baseUrl" Identify the client creation & baseUrl used
    // TODO: Replace the "baseUrl" with already tagged sinks
    val ClientCreationBaseUrlPattern: String      = ruleCache.getSystemConfigByKey(Constants.clientCreationBaseUrlPattern, true)
    val cacheCall          = cpg.call.where(_.nameNot("(<operator|<init).*")).l
    val apiInternalSources = cpg.literal.code("(?:\"|'|`)(" + ruleInfo.combinedRulePattern + ")(?:\"|'|`)").l
    val apis               = cacheCall.methodFullName(ClientCreationBaseUrlPattern).toList

    val uniqueDomains = getBaseUrlForFrontendApps(apis, apiInternalSources)
//    val apiCalls = cacheCall
//      .name(APISINKS_REGEX)
//      .methodFullNameNot(COMMON_IGNORED_SINKS_REGEX)
//      .methodFullName(commonHttpPackages)
//      .l
    sinkTaggerWithDomains(apiInternalSources, apis, builder, ruleInfo, ruleCache, uniqueDomains)

    // Identification of script tag with pixel code <Script src="https://c.amazon-adsystem.com/aax2/apstag.js" strategy="lazyOnload" />
    // Tag the respective templateDom node as API sink
    val scriptTags =
      cpg.templateDom.name(Constants.jsxElement).code("(?i)[\\\"]*<script.*" + ruleInfo.combinedRulePattern + ".*").l
    scriptTags.foreach(scriptTag => {
      var newRuleIdToUse = ruleInfo.id
      val domain         = getDomainFromTemplates(scriptTag.code)
      if (ruleInfo.id.equals(Constants.internalAPIRuleId)) addRuleTags(builder, scriptTag, ruleInfo, ruleCache)
      else {
        newRuleIdToUse = ruleInfo.id + "." + domain._2
        ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain._2))
        addRuleTags(builder, scriptTag, ruleInfo, ruleCache, Some(newRuleIdToUse))
      }
      storeForTag(builder, scriptTag, ruleCache)(Constants.apiUrl + newRuleIdToUse, domain._1)
    })

    // Identification of script tag generated dynamically in code
    // const n = document.createElement("script");
    // n.type = "text/javascript";
    // n.async = !0;
    // n.src = "https://cdn.segment.com/analytics.js/v1/" + t + "/analytics.min.js";
    // Tag the respective templateDom node as API sink
    val parentBlocksOfHTMLScriptElement = cpg
      .call(Operators.fieldAccess)
      .code(".*\\.src.*")
      .argument
      .isIdentifier
      .filter((i) => i.typeFullName.contains(Constants.HTMLScriptElement))
      .parentBlock
      .id
      .l

    val scriptLinks =
      cpg
        .literal(".*" + ruleInfo.combinedRulePattern + ".*")
        .filter((i) => parentBlocksOfHTMLScriptElement.indexOf(i.parentBlock.id.headOption.getOrElse(-1L)) > -1)
        .l

    scriptLinks.foreach((link) => {
      var newRuleIdToUse = ruleInfo.id
      val domain         = getDomainFromTemplates(link.code)
      val callTag        = link.astParent
      if (ruleInfo.id.equals(Constants.internalAPIRuleId)) addRuleTags(builder, callTag, ruleInfo, ruleCache)
      else {
        newRuleIdToUse = ruleInfo.id + "." + domain._2
        ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain._2))
        addRuleTags(builder, callTag, ruleInfo, ruleCache, Some(newRuleIdToUse))
      }
      storeForTag(builder, callTag, ruleCache)(Constants.apiUrl + newRuleIdToUse, domain._1)
    })
  }

  def getBaseUrlForFrontendApps(apis: List[CfgNode], apiInternalSources: List[AstNode])(implicit
    engineContext: EngineContext
  ): List[String] = {
    val domains = ListBuffer[String]()
    if (apis.nonEmpty && apiInternalSources.nonEmpty) {
      val apiFlows = apis.reachableByFlows(apiInternalSources)(engineContext).toList
      apiFlows.foreach(flow => {
        val literalCode = flow.elements.head.originalPropertyValue.getOrElse(flow.elements.head.code.split(" ").last)
        val domain      = getDomainFromString(literalCode)
        if (!domains.contains(domain)) {
          domains += domain
        }
      })
    }
    domains.toList
  }

  def sinkTaggerWithDomains(
    apiInternalSources: List[AstNode],
    apis: List[CfgNode],
    builder: BatchedUpdate.DiffGraphBuilder,
    ruleInfo: RuleInfo,
    ruleCache: RuleCache,
    domains: List[String],
    showAPI: Boolean = true
  )(implicit engineContext: EngineContext): Unit = {
    val filteredSourceNode =
      apiInternalSources.filter(node => isFileProcessable(getFileNameForNode(node), ruleCache))

    def tagSinkNode(apiNode: AstNode, ruleInfo: RuleInfo, domain: String): Unit = {
      val newRuleIdToUse = ruleInfo.id + "." + domain
      ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain))
      addRuleTags(builder, apiNode, ruleInfo, ruleCache, Some(newRuleIdToUse))
      storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + newRuleIdToUse, domain)
    }

    domains.foreach(i => println(i))
    if (apis.nonEmpty && filteredSourceNode.nonEmpty) {
      val apiFlows = {
        val flows = apis.reachableByFlows(filteredSourceNode)(engineContext).toList
        if (ScanProcessor.config.disableDeDuplication)
          flows
        else
          DuplicateFlowProcessor.getUniquePathsAfterDedup(flows)
      }

      // Add url as 'API' for non Internal api nodes, so that at-least we show API without domains
      if (showAPI) {// && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
        val literalPathApiNodes        = apiFlows.map(_.elements.last).toSet
        val apiNodesWithoutLiteralPath = apis.toSet.diff(literalPathApiNodes)
        apiNodesWithoutLiteralPath.foreach(apiNode => {
          domains.foreach(domain => {
            tagSinkNode(apiNode, ruleInfo, domain)
          })
        })
      }
    } // Add url as 'API' for non Internal api nodes, for cases where there is no http literal present in source code
    else if (showAPI && filteredSourceNode.isEmpty && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
      apis.foreach(apiNode => {
        domains.foreach(domain => {
          tagSinkNode(apiNode, ruleInfo, domain)
        })
      })
    }
  }

}
