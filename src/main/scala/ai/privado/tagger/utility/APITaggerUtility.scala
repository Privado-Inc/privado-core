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
import ai.privado.entrypoint.ScanProcessor
import ai.privado.languageEngine.java.language.NodeToProperty
import ai.privado.languageEngine.java.semantic.SemanticGenerator
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.utility.Utilities.{
  addRuleTags,
  getDomainFromString,
  getFileNameForNode,
  isFileProcessable,
  storeForTag
}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, CfgNode}
import overflowdb.BatchedUpdate
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._

object APITaggerUtility {
  implicit val engineContext: EngineContext =
    EngineContext(semantics = SemanticGenerator.getDefaultSemantics, config = EngineConfig(4))

  def sinkTagger(
    apiInternalSinkPattern: List[AstNode],
    apis: List[CfgNode],
    builder: BatchedUpdate.DiffGraphBuilder,
    ruleInfo: RuleInfo,
    showAPI: Boolean = true
  ): Unit = {
    val filteredSourceNode = apiInternalSinkPattern.filter(node => isFileProcessable(getFileNameForNode(node)))
    if (apis.nonEmpty && filteredSourceNode.nonEmpty) {
      val apiFlows = {
        val flows = apis.reachableByFlows(filteredSourceNode).l
        if (ScanProcessor.config.disableDeDuplication)
          flows
        else
          DuplicateFlowProcessor.getUniquePathsAfterDedup(flows)
      }
      apiFlows.foreach(flow => {
        val sourceNode = Traversal(flow.elements.head).l
        val literalCode = {
          if (sourceNode.isCall.name(Constants.runtimeString).nonEmpty) {
            sourceNode.isCall
              .name(Constants.runtimeString)
              .head
              .astChildren
              .map(node => {
                if (node.isLiteral)
                  node.code.stripSuffix("\"").stripPrefix("\"")
                else {
                  // Ex - In javascript `/user/${user.userId}/paymentDetails` needs to be represented as
                  // /user/${user[userId]}/paymentDetails due to Andromeda constraint for now
                  // Will have to remove this special handling once fix is ready in andromeda
                  "${" + {
                    val splitByDot = node.code.split("\\.")
                    if (splitByDot.size <= 1)
                      node.code
                    else
                      splitByDot(0) + splitByDot.slice(1, splitByDot.size).mkString("[", "", "]")
                  } + "}"
                }
              })
              .mkString("\"", "", "\"")
          } else
            sourceNode.head.originalPropertyValue.getOrElse(sourceNode.head.code.split(" ").last)
        }
        val apiNode = flow.elements.last
        // Tag API's when we find a dataflow to them
        var newRuleIdToUse = ruleInfo.id
        if (ruleInfo.id.equals(Constants.internalAPIRuleId))
          addRuleTags(builder, apiNode, ruleInfo)
        else {
          val domain = getDomainFromString(literalCode)
          newRuleIdToUse = ruleInfo.id + "." + domain
          RuleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain))
          addRuleTags(builder, apiNode, ruleInfo, Some(newRuleIdToUse))
        }
        storeForTag(builder, apiNode)(Constants.apiUrl + newRuleIdToUse, literalCode)
      })
      // Add url as 'API' for non Internal api nodes, so that at-least we show API without domains
      if (showAPI && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
        val literalPathApiNodes        = apiFlows.map(_.elements.last).toSet
        val apiNodesWithoutLiteralPath = apis.toSet.diff(literalPathApiNodes)
        apiNodesWithoutLiteralPath.foreach(apiNode => {
          addRuleTags(builder, apiNode, ruleInfo)
          storeForTag(builder, apiNode)(Constants.apiUrl + ruleInfo.id, Constants.API)
        })
      }
    } // Add url as 'API' for non Internal api nodes, for cases where there is no http literal present in source code
    else if (showAPI && filteredSourceNode.isEmpty && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
      apis.foreach(apiNode => {
        addRuleTags(builder, apiNode, ruleInfo)
        storeForTag(builder, apiNode)(Constants.apiUrl + ruleInfo.id, Constants.API)
      })
    }
  }

  def cleanUrl(urlValue: String): String = {
    urlValue.stripPrefix("\"").stripSuffix("\"")
  }

}
