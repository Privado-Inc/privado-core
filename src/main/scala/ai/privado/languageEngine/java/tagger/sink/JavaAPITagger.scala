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
package ai.privado.languageEngine.java.tagger.sink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.language.*
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.languageEngine.java.tagger.Utility.{GRPCTaggerUtility, SOAPTaggerUtility}
import ai.privado.metric.MetricHandler
import ai.privado.model.{Constants, InternalTag, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.utility.APITaggerUtility.{SERVICE_URL_REGEX_PATTERN, sinkTagger}
import ai.privado.utility.{ImportUtility, Utilities}
import io.circe.Json
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.util.Calendar
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.SetIsParallelizable
import scala.util.matching.Regex

/*
  Enum class to represent the different API Tagger versions being used for Java
  Skip Tagger -> The tagger is being skipped to prevent False Positive Results
  V1 Tagger -> The brute implementation of finding sinks is being used
  V2 Tagger -> The approach uses most common HTTP Packages for sinks in Java
 */
object APITaggerVersionJava extends Enumeration {
  type APITaggerVersionJava = Value
  val SkipTagger, V1Tagger, V2Tagger = Value
}

class JavaAPITagger(cpg: Cpg, ruleCache: RuleCache, privadoInputConfig: PrivadoInput, appCache: AppCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  implicit val engineContext: EngineContext =
    Utilities.getEngineContext(privadoInputConfig, appCache, 4)(JavaSemanticGenerator.getDefaultSemantics)
  val cacheCall: List[Call]                      = cpg.call.where(_.nameNot("(<operator|<init).*")).l
  val internalMethodCall: List[String]           = cpg.method.dedup.isExternal(false).fullName.take(30).l
  val topMatch: mutable.HashMap[String, Integer] = mutable.HashMap[String, Integer]()

  val COMMON_IGNORED_SINKS_REGEX = ruleCache.getSystemConfigByKey(Constants.ignoredSinks)

  lazy val APISINKS_REGEX = ruleCache.getSystemConfigByKey(Constants.apiSinks)

  internalMethodCall.foreach((method) => {
    val key     = method.split("[.:]").take(2).mkString(".")
    val currVal = topMatch.getOrElse(key, 0).asInstanceOf[Int]
    topMatch(key) = (currVal + 1)
  })

  val APISINKS_IGNORE_REGEX = topMatch.keys.mkString("^(", "|", ").*")
  var apis = cacheCall
    .name(APISINKS_REGEX)
    .methodFullNameNot(COMMON_IGNORED_SINKS_REGEX)
    .l

  lazy val apiTaggerToUse = apis.methodFullName(s"${commonHttpPackages}${APISINKS_REGEX}.*").l.size match {
    case 0 =>
      if (isPackageInImport(commonHttpPackages.r)) {
        MetricHandler.metricsData("apiTaggerVersion") = Json.fromString(APITaggerVersionJava.V1Tagger.toString)
        // filter out the internal package sinks to reduce False positives only for V1 tagger
        apis = apis.methodFullNameNot(APISINKS_IGNORE_REGEX).l
        APITaggerVersionJava.V1Tagger
      } else {
        MetricHandler.metricsData("apiTaggerVersion") = Json.fromString(APITaggerVersionJava.SkipTagger.toString)
        APITaggerVersionJava.SkipTagger
      }
    case _ =>
      MetricHandler.metricsData("apiTaggerVersion") = Json.fromString(APITaggerVersionJava.V2Tagger.toString)
      APITaggerVersionJava.V2Tagger
  }

  val commonHttpPackages: String = ruleCache.getSystemConfigByKey(Constants.apiHttpLibraries)
  val grpcSinks                  = GRPCTaggerUtility.getGrpcSinks(cpg)
  val soapSinks                  = SOAPTaggerUtility.getAPICallNodes(cpg)

  override def generateParts(): Array[_ <: AnyRef] = {
    ruleCache.getAllRuleInfo
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val apiInternalSources = cpg.literal.code("(?:\"|')(" + ruleInfo.combinedRulePattern + ")(?:\"|')").l
    val propertySources    = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l
    val serviceSource      = cpg.property.filter(p => p.value matches SERVICE_URL_REGEX_PATTERN).usedAt.l

    // Support to use `identifier` in API's
    val identifierRegex = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
    val identifierSource = {
      if (!ruleInfo.id.equals(Constants.internalAPIRuleId))
        cpg.identifier(identifierRegex).l ++ cpg.member
          .name(identifierRegex)
          .l ++ cpg.property.filter(p => p.name matches (identifierRegex)).usedAt.l
      else
        List()
    }

    // To handle feign implementation, run if feign found in any of the imports
    val feignAPISinks = {
      if (isPackageInImport(".*(?i)feign.*".r))
        new FeignAPI(cpg, ruleCache).tagFeignAPIWithDomainAndReturnWithoutDomainAPISinks(
          builder,
          ruleInfo,
          apiInternalSources ++ propertySources ++ identifierSource,
          privadoInputConfig,
          appCache
        )
      else
        List()
    }

    val markedAPISinks = cpg.call
      .where(_.tag.nameExact(InternalTag.API_SINK_MARKED.toString))
      .whereNot(_.tag.nameExact(InternalTag.API_URL_MARKED.toString))
      .l

    apiTaggerToUse match {
      case APITaggerVersionJava.V1Tagger =>
        logger.debug("Using brute API Tagger to find API sinks")
        println(s"${Calendar.getInstance().getTime} - --API TAGGER V1 invoked...")
        sinkTagger(
          apiInternalSources ++ propertySources ++ identifierSource ++ serviceSource,
          apis,
          builder,
          ruleInfo,
          ruleCache,
          privadoInputConfig,
          privadoInputConfig.enableAPIDisplay
        )
        sinkTagger(
          apiInternalSources ++ propertySources ++ identifierSource ++ serviceSource,
          feignAPISinks ++ grpcSinks ++ soapSinks ++ markedAPISinks,
          builder,
          ruleInfo,
          ruleCache,
          privadoInputConfig
        )
      case APITaggerVersionJava.V2Tagger =>
        logger.debug("Using Enhanced API tagger to find API sinks")
        println(s"${Calendar.getInstance().getTime} - --API TAGGER V2 invoked...")
        sinkTagger(
          apiInternalSources ++ propertySources ++ identifierSource ++ serviceSource,
          apis.methodFullName(commonHttpPackages).l ++ feignAPISinks ++ grpcSinks ++ soapSinks ++ markedAPISinks,
          builder,
          ruleInfo,
          ruleCache,
          privadoInputConfig
        )
      case _ =>
        logger.debug("Skipping API Tagger because valid match not found, only applying Feign client")
        println(s"${Calendar.getInstance().getTime} - --API TAGGER SKIPPED, applying Feign client API...")
        sinkTagger(
          apiInternalSources ++ propertySources ++ identifierSource ++ serviceSource,
          feignAPISinks ++ grpcSinks ++ soapSinks ++ markedAPISinks,
          builder,
          ruleInfo,
          ruleCache,
          privadoInputConfig
        )
    }
  }

  /** Checks if the given regex matches with any import statement
    * @param packageRegex
    * @return
    */
  private def isPackageInImport(packageRegex: Regex): Boolean =
    ImportUtility
      .getAllImportsFromProject(appCache.scanPath, Language.JAVA, ruleCache)
      .par
      .map(packageRegex.findFirstIn)
      .nonEmpty
}
