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

import ai.privado.cache.{AppCache, FileLinkingMetadata, RuleCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.language.{NodeStarters, StepsForProperty}
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.model.{CatLevelOne, Constants, InternalTag, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.utility.APITaggerUtility.{SERVICE_URL_REGEX_PATTERN, sinkTagger}
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class APITagger(
  cpg: Cpg,
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  appCache: AppCache,
  fileLinkingMetadata: FileLinkingMetadata
) extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  val cacheCall = cpg.call.where(_.nameNot("(<operator|<init).*")).l

  val COMMON_IGNORED_SINKS_REGEX = ruleCache.getSystemConfigByKey(Constants.ignoredSinks)
  lazy val APISINKS_REGEX        = ruleCache.getSystemConfigByKey(Constants.apiSinks)
  val commonHttpPackages: String = ruleCache.getSystemConfigByKey(Constants.apiHttpLibraries)

  // NOTE : JSAPITagger is overriding it to run the query on cpg.code instead of methodFullName
  val apis = cacheCall
    .name(APISINKS_REGEX)
    .methodFullNameNot(COMMON_IGNORED_SINKS_REGEX)
    .methodFullName(commonHttpPackages)
    .l

  implicit val engineContext: EngineContext =
    Utilities.getEngineContext(PrivadoInput(disableDeDuplication = true), appCache = appCache, 4)(
      JavaSemanticGenerator.getDefaultSemantics
    )

  override def generateParts(): Array[_ <: AnyRef] = {
    ruleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .filterNot(_.isGenerated) // Filter out generated rules, we only need to use the passed rules
      .toArray
  }
  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val apiInternalSources = cpg.literal.code("(?:\"|'|`)(" + ruleInfo.combinedRulePattern + ")(?:\"|'|`)").l
    val propertySources    = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l
    val identifierRegex    = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
    val serviceSource      = cpg.property.filter(p => p.value matches SERVICE_URL_REGEX_PATTERN).usedAt.l
    val identifierSource = {
      if (!ruleInfo.id.equals(Constants.internalAPIRuleId))
        cpg.identifier(identifierRegex).l ++ cpg.member
          .name(identifierRegex)
          .l ++ cpg.property.filter(p => p.name matches (identifierRegex)).usedAt.l
      else
        List()
    }
    sinkTagger(
      cpg,
      apiInternalSources ++ propertySources ++ identifierSource ++ serviceSource,
      apis,
      builder,
      ruleInfo,
      ruleCache,
      privadoInput,
      fileLinkingMetadata
    )
  }
}
