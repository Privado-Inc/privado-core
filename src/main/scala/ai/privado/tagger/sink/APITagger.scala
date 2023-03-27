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

package ai.privado.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.language.{NodeStarters, StepsForProperty}
import ai.privado.model.{Constants, NodeType, RuleInfo}
import ai.privado.tagger.utility.APITaggerUtility.sinkTagger
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.DefaultSemantics
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable.HashMap

class APITagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {

  val cacheCall                  = cpg.call.where(_.nameNot("(<operator|<init).*")).l
  val internalMethodCall         = cpg.method.dedup.isExternal(false).fullName.take(30).l
  val topMatch                   = HashMap[String, Integer]()
  val COMMON_IGNORED_SINKS_REGEX = "(?i).*(?<=map|list|jsonobject|json|array|arrays).*(put:|get:).*"

  internalMethodCall.foreach((method) => {
    val key     = method.split("[.:]").take(2).mkString(".")
    val currVal = topMatch.getOrElse(key, 0).asInstanceOf[Int]
    topMatch(key) = (currVal + 1)
  })
  var APISINKS_IGNORE_REGEX = "^("
  topMatch.foreach((mapEntry) => {
    if (mapEntry == topMatch.last) {
      APISINKS_IGNORE_REGEX += mapEntry._1 + ").*"
    } else {
      APISINKS_IGNORE_REGEX += mapEntry._1 + "|"
    }
  })
  val apis = cacheCall
    .name(APISINKS_REGEX)
    .methodFullNameNot(APISINKS_IGNORE_REGEX)
    .methodFullNameNot(COMMON_IGNORED_SINKS_REGEX)
    .l

  implicit val engineContext: EngineContext = EngineContext(semantics = DefaultSemantics(), config = EngineConfig(4))

  lazy val APISINKS_REGEX =
    "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|fetch|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|" +
      "sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity|postForObject)"

  lazy val APISINKSIGNORE_REGEX = "(?i)(json|map).*(put:|get:)"

  override def generateParts(): Array[_ <: AnyRef] = {
    RuleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toArray
  }
  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val apiInternalSources = cpg.literal
      .code("(?:\"|')(" + ruleInfo.combinedRulePattern + ")(?:\"|')")
      .map(literalNode => {
        // Runtime string parent is used to consider string which are generated at runtime
        // Ex - In javascript `/user/${userId}/paymentDetails`
        val runtimeStringParent = Traversal(literalNode).astParent.isCall.name(Constants.runtimeString).l
        if (runtimeStringParent.nonEmpty) {
          runtimeStringParent.head
        } else
          literalNode
      })
      .l
    val propertySources = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l
    sinkTagger(apiInternalSources ++ propertySources, apis, builder, ruleInfo)
  }
}
