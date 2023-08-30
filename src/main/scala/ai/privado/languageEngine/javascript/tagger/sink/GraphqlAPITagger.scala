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
import ai.privado.model.{Constants, NodeType, RuleInfo}
import ai.privado.tagger.sink.APITagger

import scala.collection.mutable.ListBuffer
import ai.privado.languageEngine.java.language.{NodeStarters, NodeToProperty, StepsForProperty}
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.tagger.PrivadoParallelCpgPass
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

class GraphqlAPITagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  override def generateParts(): Array[_ <: AnyRef] = {
    ruleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val commonGraphqlPackages: String = ruleCache.getSystemConfigByKey(Constants.apiGraphqlLibraries, true)
    val graphqlREADSink: String       = ruleCache.getSystemConfigByKey(Constants.apiGraphqlReadSink, true)
    val graphqlWRITESink: String      = ruleCache.getSystemConfigByKey(Constants.apiGraphqlWriteSink, true)
    val cacheCall                     = cpg.call.where(_.nameNot("(<operator|<init).*")).l
    val apis                          = cacheCall.methodFullName(commonGraphqlPackages).toList

    if (!ruleInfo.id.equals(Constants.internalAPIRuleId)) {
      apis.foreach((apiNode) => {
        val isReadAPI  = apiNode.name.matches(graphqlREADSink)
        val isWriteAPI = apiNode.name.matches(graphqlWRITESink)

        if (isReadAPI) {
          val newRuleIdToUse = ruleInfo.id + " (Read)"
          ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " (Read)"))
          addRuleTags(builder, apiNode, ruleInfo, ruleCache, Some(newRuleIdToUse))
          storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + newRuleIdToUse, Constants.API + " (Read)")
        } else if (isWriteAPI) {
          val newRuleIdToUse = ruleInfo.id + " (Write)"
          ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " (Write)"))
          addRuleTags(builder, apiNode, ruleInfo, ruleCache, Some(newRuleIdToUse))
          storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + newRuleIdToUse, Constants.API + " (Write)")
        } else {
          addRuleTags(builder, apiNode, ruleInfo, ruleCache)
          storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + ruleInfo.id, Constants.API)
        }
      });
    }
  }

}
