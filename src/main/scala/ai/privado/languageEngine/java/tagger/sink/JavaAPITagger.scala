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
import ai.privado.languageEngine.java.language.{NodeStarters, NodeToProperty, StepsForProperty}
import ai.privado.metric.MetricHandler
import ai.privado.model.{Constants, NodeType, RuleInfo}
import ai.privado.utility.ImportUtility
import ai.privado.model.Language
import ai.privado.utility.Utilities.{
  addRuleTags,
  getDefaultSemantics,
  getFileNameForNode,
  isFileProcessable,
  storeForTag
}
import io.circe.Json
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.SetIsParallelizable

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

class JavaAPITagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  private val logger     = LoggerFactory.getLogger(this.getClass)
  val cacheCall          = cpg.call.where(_.nameNot("(<operator|<init).*")).l
  val internalMethodCall = cpg.method.dedup.isExternal(false).fullName.take(30).l
  val topMatch           = mutable.HashMap[String, Integer]()

  val COMMON_IGNORED_SINKS_REGEX = "(?i).*(?<=map|list|jsonobject|json|array|arrays).*(put:|get:).*"
  lazy val APISINKS_REGEX =
    "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|fetch|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|" +
      "sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity)"

  internalMethodCall.foreach((method) => {
    val key     = method.split("[.:]").take(2).mkString(".")
    val currVal = topMatch.getOrElse(key, 0).asInstanceOf[Int]
    topMatch(key) = (currVal + 1)
  })

  val APISINKS_IGNORE_REGEX = topMatch.keys.mkString("^(", "|", ").*")
  val apis = cacheCall
    .name(APISINKS_REGEX)
    .methodFullNameNot(APISINKS_IGNORE_REGEX)
    .methodFullNameNot(COMMON_IGNORED_SINKS_REGEX)
    .l

  lazy val apiTaggerToUse = apis.methodFullName(s"${commonHttpPackages}${APISINKS_REGEX}.*").l.size match {
    case 0 =>
      if (httpPackagesInImport()) {
        MetricHandler.metricsData("apiTaggerVersion") = Json.fromString(APITaggerVersionJava.V1Tagger.toString)
        APITaggerVersionJava.V1Tagger
      } else {
        MetricHandler.metricsData("apiTaggerVersion") = Json.fromString(APITaggerVersionJava.SkipTagger.toString)
        APITaggerVersionJava.SkipTagger
      }
    case _ =>
      MetricHandler.metricsData("apiTaggerVersion") = Json.fromString(APITaggerVersionJava.V2Tagger.toString)
      APITaggerVersionJava.V2Tagger
  }

  implicit val engineContext: EngineContext = EngineContext(semantics = getDefaultSemantics, config = EngineConfig(4))
  val commonHttpPackages: String =
    "^(?i)(org.apache.http|okhttp|org.glassfish.jersey|com.mashape.unirest|java.net.http|java.net.URL|org.springframework.(web|core.io)|groovyx.net.http|org.asynchttpclient|kong.unirest.java|org.concordion.cubano.driver.http|javax.net.ssl).*"

  override def generateParts(): Array[_ <: AnyRef] = {
    RuleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val apiInternalSinkPattern = cpg.literal.code("(?:\"|')(" + ruleInfo.combinedRulePattern + ")(?:\"|')").l
    val propertySinks          = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l
    apiTaggerToUse match {
      case APITaggerVersionJava.V1Tagger =>
        logger.debug("Using brute API Tagger to find API sinks")
        sinkTagger(apiInternalSinkPattern, apis, builder, ruleInfo)
        sinkTagger(propertySinks, apis, builder, ruleInfo)
      case APITaggerVersionJava.V2Tagger =>
        logger.debug("Using Enhanced API tagger to find API sinks")
        sinkTagger(apiInternalSinkPattern, apis.methodFullName(commonHttpPackages).l, builder, ruleInfo)
        sinkTagger(propertySinks, apis.methodFullName(commonHttpPackages).l, builder, ruleInfo)
      case _ =>
        logger.debug("Skipping API Tagger because valid match not found")
    }
  }

  private def sinkTagger(
    apiInternalSinkPattern: List[CfgNode],
    apis: List[CfgNode],
    builder: BatchedUpdate.DiffGraphBuilder,
    ruleInfo: RuleInfo
  ): Unit = {
    val filteredLiteralSourceNode = apiInternalSinkPattern.filter(node => isFileProcessable(getFileNameForNode(node)))
    if (apis.nonEmpty && filteredLiteralSourceNode.nonEmpty) {
      val apiFlows = apis.reachableByFlows(filteredLiteralSourceNode).l
      apiFlows.foreach(flow => {
        val literalCode = flow.elements.head.originalPropertyValue.getOrElse(flow.elements.head.code)
        val apiNode     = flow.elements.last
        addRuleTags(builder, apiNode, ruleInfo)
        storeForTag(builder, apiNode)(Constants.apiUrl, literalCode)
      })
    }
  }

  private def httpPackagesInImport(): Boolean = {
    val imports          = ImportUtility.getAllImportsFromProject(AppCache.localScanPath, Language.JAVA)
    val httpPackageRegex = commonHttpPackages.r
    val httpPackages     = mutable.HashSet[String]()

    imports.par.foreach((statement) => {
      httpPackageRegex.findFirstIn(statement) match {
        case Some(value) => httpPackages.addOne(value)
        case _           => ()
      }
    })

    httpPackages.size match {
      case 0 => true
      case _ => false
    }
  }

}
