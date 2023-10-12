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
package ai.privado.languageEngine.go.tagger.sink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.language.{NodeStarters, StepsForProperty}
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.metric.MetricHandler
import ai.privado.model.{Constants, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.utility.APITaggerUtility.sinkTagger
import ai.privado.utility.Utilities
import io.circe.Json
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.util.Calendar

class GoAPITagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  val cacheCall = cpg.call.where(_.nameNot("(<operator).*")).l

  lazy val APISINKS_REGEX = ruleCache.getSystemConfigByKey(Constants.apiSinks)

  val apis = cacheCall.name(APISINKS_REGEX).l

  MetricHandler.metricsData("apiTaggerVersion") = Json.fromString("Common HTTP Libraries Used")

  implicit val engineContext: EngineContext = Utilities.getEngineContext(privadoInput, 4)
  val commonHttpPackages: String            = ruleCache.getSystemConfigByKey(Constants.apiHttpLibraries)

  override def generateParts(): Array[_ <: AnyRef] = {
    ruleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val apiInternalSources = cpg.literal.code("(?:\"|'){0,1}(" + ruleInfo.combinedRulePattern + ")(?:\"|'){0,1}").l
    val propertySources    = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l
    // Support to use `identifier` in API's
    val identifierRegex = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
    val identifierSource = {
      if (!ruleInfo.id.equals(Constants.internalAPIRuleId))
        cpg.identifier(identifierRegex).l ++ cpg.property.filter(p => p.name matches (identifierRegex)).usedAt.l
      else
        List()
    }

    logger.debug("Using Enhanced API tagger to find API sinks")
    println(s"${Calendar.getInstance().getTime} - --API TAGGER Common HTTP Libraries Used...")
    sinkTagger(
      apiInternalSources ++ propertySources ++ identifierSource,
      apis.methodFullName(commonHttpPackages).l,
      builder,
      ruleInfo,
      ruleCache,
      privadoInput
    )
  }

}
