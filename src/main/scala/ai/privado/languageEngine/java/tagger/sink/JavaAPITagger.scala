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

import ai.privado.cache.AppCache
import ai.privado.languageEngine.java.language.{NodeStarters, NodeToProperty, StepsForProperty}
import ai.privado.metric.MetricHandler
import ai.privado.model.{Constants, Language, RuleInfo}
import ai.privado.tagger.PrivadoSimplePass
import ai.privado.utility.ImportUtility
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
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

import scala.collection.mutable

class JavaAPITagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {

  val cacheCall                  = cpg.call.where(_.nameNot("(<operator|<init).*")).l
  val internalMethodCall         = cpg.method.dedup.isExternal(false).fullName.take(30).l
  val topMatch                   = mutable.HashMap[String, Integer]()
  val COMMON_IGNORED_SINKS_REGEX = "(?i).*(?<=map|list|jsonobject|json|array|arrays).*(put:|get:).*"
  lazy val APISINKS_REGEX =
    "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|fetch|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|" +
      "sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity)"

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

  implicit val engineContext: EngineContext = EngineContext(semantics = getDefaultSemantics, config = EngineConfig(4))
  val commonHttpPackages: String =
    "^(?i)(org.apache.http|okhttp|org.glassfish.jersey|com.mashape.unirest|java.net.http|java.net.URL|org.springframework.(web|core.io)|groovyx.net.http|org.asynchttpclient|kong.unirest.java|org.concordion.cubano.driver.http|javax.net.ssl).*"
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val apiInternalSinkPattern = cpg.literal.code("(?:\"|')(" + ruleInfo.combinedRulePattern + ")(?:\"|')").l
    val propertySinks          = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l
    apis.methodFullName(s"${commonHttpPackages}${APISINKS_REGEX}.*").l.size match {
      case 0 =>
        if (httpPackagesInImport()) {
          logger.debug("Using API Tagger for finding API sinks")
          MetricHandler.metricsData("apiTaggerVersion") = Json.fromString("v1")
          sinkTagger(apiInternalSinkPattern, apis, builder, ruleInfo)
          sinkTagger(propertySinks, apis, builder, ruleInfo)
        } else {
          MetricHandler.metricsData("apiTaggerVersion") = Json.fromString("NA")
          logger.debug("Skipping API Tagger because not valid match found")
        }
      case _ => {
        logger.debug("Using Enhanced API tagger to find API sinks")
        MetricHandler.metricsData("apiTaggerVersion") = Json.fromString("v2")
        sinkTagger(apiInternalSinkPattern, apis.methodFullName(commonHttpPackages).l, builder, ruleInfo)
        sinkTagger(propertySinks, apis.methodFullName(commonHttpPackages).l, builder, ruleInfo)
      }
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
    val imports = ImportUtility.getAllImportsFromProject(AppCache.localScanPath, Language.JAVA)
    imports.size match {
      case 0 => true
      case _ => false
    }
  }

}
