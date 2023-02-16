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

import ai.privado.languageEngine.java.language.NodeToProperty
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.utility.Utilities.{
  addRuleTags,
  getDefaultSemantics,
  getFileNameForNode,
  isFileProcessable,
  storeForTag
}
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import overflowdb.BatchedUpdate
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}

object APITaggerUtility {
  implicit val engineContext: EngineContext = EngineContext(semantics = getDefaultSemantics, config = EngineConfig(4))

  def sinkTagger(
    apiInternalSinkPattern: List[CfgNode],
    apis: List[CfgNode],
    builder: BatchedUpdate.DiffGraphBuilder,
    ruleInfo: RuleInfo
  ): Unit = {
    val filteredSourceNode = apiInternalSinkPattern.filter(node => isFileProcessable(getFileNameForNode(node)))
    // Tag all api sinks irrespective of a dataflow except for Internal api's
    if (!ruleInfo.id.equals(Constants.internalAPIRuleId))
      apis.foreach(apiNode => addRuleTags(builder, apiNode, ruleInfo))
    if (apis.nonEmpty && filteredSourceNode.nonEmpty) {
      val apiFlows = apis.reachableByFlows(filteredSourceNode).l
      apiFlows.foreach(flow => {
        val literalCode = flow.elements.head.originalPropertyValue.getOrElse(flow.elements.head.code)
        val apiNode     = flow.elements.last
        // Tag internal api's only when we find a dataflow to them
        if (ruleInfo.id.equals(Constants.internalAPIRuleId))
          addRuleTags(builder, apiNode, ruleInfo)
        storeForTag(builder, apiNode)(Constants.apiUrl + ruleInfo.catLevelTwo, literalCode)
      })
      // Add url as 'API' for non Internal api nodes, so that at-least we show API without domains
      if (!ruleInfo.id.equals(Constants.internalAPIRuleId)) {
        val literalPathApiNodes        = apiFlows.map(_.elements.last).toSet
        val apiNodesWithoutLiteralPath = apis.toSet.diff(literalPathApiNodes)
        apiNodesWithoutLiteralPath.foreach(apiNode =>
          storeForTag(builder, apiNode)(Constants.apiUrl + ruleInfo.catLevelTwo, Constants.API)
        )
      }
    }
    // Add url as 'API' for non Internal api nodes, for cases where there is no http literal matched
    if (filteredSourceNode.isEmpty && !ruleInfo.id.equals(Constants.internalAPIRuleId)) {
      apis.foreach(apiNode => storeForTag(builder, apiNode)(Constants.apiUrl + ruleInfo.catLevelTwo, Constants.API))
    }
  }

}
