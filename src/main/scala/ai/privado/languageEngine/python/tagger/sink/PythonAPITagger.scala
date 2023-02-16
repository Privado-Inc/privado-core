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
package ai.privado.languageEngine.python.tagger.sink

import ai.privado.cache.{RuleCache}
import ai.privado.languageEngine.java.language.{NodeStarters, StepsForProperty}
import ai.privado.metric.MetricHandler
import ai.privado.model.{NodeType, RuleInfo}
import ai.privado.tagger.utility.APITaggerUtility.sinkTagger
import ai.privado.utility.Utilities.getDefaultSemantics
import io.circe.Json
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import java.util.Calendar

class PythonAPITagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  val cacheCall      = cpg.call.where(_.nameNot("(<operator|<init).*")).l

  val commonIgnoredSinks = RuleCache.getSystemConfigByKey("ignoredSinks")
  val apiSinksRegex      = RuleCache.getSystemConfigByKey("apiSinks")

  lazy val APISINKS_REGEX = apiSinksRegex.size match {
    case 0 =>
      "(?i).*(?:url|client|get|set|post|put|patch|delete|head|options|request|feed|trigger|init|find|send|receive|redirect|fetch|execute|response|pool|client|http|load|list|trace|remove|write|provider|host|access|info_read|select|perform).*"
    case _ => apiSinksRegex.map(config => config.value).mkString("(?i)(", "|", ")")
  }

  val apis = cacheCall.name(APISINKS_REGEX).l

  MetricHandler.metricsData("apiTaggerVersion") = Json.fromString("Common HTTP Libraries Used")

  implicit val engineContext: EngineContext = EngineContext(semantics = getDefaultSemantics, config = EngineConfig(4))
  val systemConfigHttpLibraries             = RuleCache.getSystemConfigByKey("apiHttpLibraries")
  val commonHttpPackages: String = {
    systemConfigHttpLibraries.size match {
      case 0 =>
        "(?i)(request|aiohttp|treq|grequests|urllib|http|uplink|httoop|flask_restful|tornado.httpclient|pycurl|bs4|.*(HttpClient)).*"
      case _ => systemConfigHttpLibraries.map(config => config.value).mkString("(?i)(", "|", ")")
    }
  }

  override def generateParts(): Array[_ <: AnyRef] = {
    RuleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val apiInternalSinkPattern = cpg.literal.code("(?:\"|'){0,1}(" + ruleInfo.combinedRulePattern + ")(?:\"|'){0,1}").l
    val propertySinks          = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l

    logger.debug("Using Enhanced API tagger to find API sinks")
    println(s"${Calendar.getInstance().getTime} - --API TAGGER Common HTTP Libraries Used...")
    sinkTagger(apiInternalSinkPattern, apis.methodFullName(commonHttpPackages).l, builder, ruleInfo)
    sinkTagger(propertySinks, apis.methodFullName(commonHttpPackages).l, builder, ruleInfo)
  }

}
