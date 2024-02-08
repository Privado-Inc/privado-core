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
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.sink.APITagger

import scala.collection.mutable.ListBuffer
import ai.privado.languageEngine.java.language.{NodeStarters, NodeToProperty, StepsForProperty}
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode}
import io.joern.dataflowengineoss.DefaultSemantics
import ai.privado.utility.Utilities.{
  addRuleTags,
  getDomainFromString,
  getDomainFromTemplates,
  getAPIIdentifierFromCode,
  getFileNameForNode,
  isFileProcessable,
  storeForTag
}
import io.joern.dataflowengineoss.language.toExtendedCfgNode
import overflowdb.BatchedUpdate

class JSAPITagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput)
    extends APITagger(cpg, ruleCache, privadoInput) {

  override val apis = cacheCall
    .name(APISINKS_REGEX)
    .methodFullNameNot(COMMON_IGNORED_SINKS_REGEX)
    .code(commonHttpPackages)
    .l

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    super.runOnPart(builder, ruleInfo)

    // baseUrl" Identify the client creation & baseUrl used
    // Tagging below two cases:
    // 1. Axios Create case
    //    const axios = require("axios");
    //    const baseUrl = 'http://twitter.com:4001';
    //    const axiosInstance = axios.create({
    //      baseURL: baseUrl
    //    });
    // 2. ANgular Interceptor case
    //    @Injectable({ providedIn: "root" })
    //    export class ApiInterceptor
    //    implements HttpInterceptor {
    //      intercept(req: HttpRequest < any >, next: HttpHandler): Observable < HttpEvent < any >> {
    //        const apiReq = req.clone({
    //          url: `https://api.realworld.io/api${req.url}`
    //        });
    //        return next.handle(apiReq);
    //      }
    //    }

    val clientCreationBaseUrlPattern: String =
      ruleCache.getSystemConfigByKey(Constants.clientCreationBaseUrlPattern, true)
    val apiLiterals     = cpg.literal.code("(?:\"|'|`)(" + ruleInfo.combinedRulePattern + ")(?:\"|'|`)").l
    val identifierRegex = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
    val initApiCalls    = cacheCall.methodFullName(clientCreationBaseUrlPattern).toList
    val uniqueDomains   = getBaseUrlForFrontendApps(initApiCalls, apiLiterals, builder, ruleInfo, ruleCache)
    // TODO: Need another approach to map the baseUrl with actual sink nodes

    // Identification of script tag with pixel code <Script src="https://c.amazon-adsystem.com/aax2/apstag.js" strategy="lazyOnload" />
    // Tag the respective templateDom node as API sink
    val scriptTags =
      cpg.templateDom
        .name(s"(?i)(${Constants.jsxElement}|${Constants.HTMLElement})")
        .code("(?i)[\\\"]*<(script|iframe).*(" + ruleInfo.combinedRulePattern + "|" + identifierRegex + ").*")
        .l
    scriptTags.dedup.foreach(scriptTag => {
      var newRuleIdToUse = ruleInfo.id
      val domain         = getDomainFromTemplates(scriptTag.code)
      if (!domain._1.equals(Constants.UnknownDomain)) {
        if (ruleInfo.id.equals(Constants.internalAPIRuleId)) addRuleTags(builder, scriptTag, ruleInfo, ruleCache)
        else {
          newRuleIdToUse = ruleInfo.id + "." + domain._2
          ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain._2))
          addRuleTags(builder, scriptTag, ruleInfo, ruleCache, Some(newRuleIdToUse))
        }
        storeForTag(builder, scriptTag, ruleCache)(Constants.apiUrl + newRuleIdToUse, domain._1)
      } else {
        val identifierDomain =
          getAPIIdentifierFromCode(scriptTag.code, identifierRegex).getOrElse(Constants.UnknownDomain)
        if (!identifierDomain.equals(Constants.UnknownDomain)) {
          if (!ruleInfo.id.equals(Constants.internalAPIRuleId)) {
            newRuleIdToUse = ruleInfo.id + "." + identifierDomain
            ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + identifierDomain))
            addRuleTags(builder, scriptTag, ruleInfo, ruleCache, Some(newRuleIdToUse))
            storeForTag(builder, scriptTag, ruleCache)(Constants.apiUrl + newRuleIdToUse, identifierDomain)
          }
        }
      }
    })

    // Identification of script tags from loadExternalScript() method
    // await loadExternalScript(`https://widget.intercom.io/widget/${INTERCOM_BOT_ID}`, 'Intercom');
    val loadExternalScriptCalls = cpg.call("(loadExternalScript|loadThirdPartyScript)").l

    loadExternalScriptCalls.foreach(externalScriptCall => {
      var newRuleIdToUse = ruleInfo.id
      val domain         = getDomainFromTemplates(externalScriptCall.code)
      if (ruleInfo.id.equals(Constants.internalAPIRuleId)) addRuleTags(builder, externalScriptCall, ruleInfo, ruleCache)
      else {
        newRuleIdToUse = ruleInfo.id + "." + domain._2
        ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain._2))
        addRuleTags(builder, externalScriptCall, ruleInfo, ruleCache, Some(newRuleIdToUse))
      }
      storeForTag(builder, externalScriptCall, ruleCache)(Constants.apiUrl + newRuleIdToUse, domain._1)
    })

    // Identification of script tag generated dynamically in code
    // const n = document.createElement("script");
    // n.type = "text/javascript";
    // n.async = !0;
    // n.src = "https://cdn.segment.com/analytics.js/v1/" + t + "/analytics.min.js";
    // Tag the respective templateDom node as API sink
    val parentBlocksOfHTMLScriptElement = cpg
      .call(Operators.fieldAccess)
      .code(".*\\.src.*")
      .argument
      .isIdentifier
      .filter((i) => i.typeFullName.contains(Constants.HTMLScriptElement))
      .parentBlock
      .id
      .l

    val scriptLinks =
      cpg
        .literal(".*" + ruleInfo.combinedRulePattern + ".*")
        .filter((i) => parentBlocksOfHTMLScriptElement.indexOf(i.parentBlock.id.headOption.getOrElse(-1L)) > -1)
        .l

    scriptLinks.foreach((link) => {
      var newRuleIdToUse = ruleInfo.id
      val domain         = getDomainFromTemplates(link.code)
      val callTag        = link.astParent
      if (ruleInfo.id.equals(Constants.internalAPIRuleId)) addRuleTags(builder, callTag, ruleInfo, ruleCache)
      else {
        newRuleIdToUse = ruleInfo.id + "." + domain._2
        ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain._2))
        addRuleTags(builder, callTag, ruleInfo, ruleCache, Some(newRuleIdToUse))
      }
      storeForTag(builder, callTag, ruleCache)(Constants.apiUrl + newRuleIdToUse, domain._1)
    })
  }

  def getBaseUrlForFrontendApps(
    apis: List[CfgNode],
    apiInternalSources: List[AstNode],
    builder: BatchedUpdate.DiffGraphBuilder,
    ruleInfo: RuleInfo,
    ruleCache: RuleCache
  )(implicit engineContext: EngineContext): List[String] = {
    val domains = ListBuffer[String]()
    if (apis.nonEmpty && apiInternalSources.nonEmpty) {
      val apiFlows = apis.reachableByFlows(apiInternalSources)(engineContext).toList
      apiFlows.foreach(flow => {
        val literalCode = flow.elements.head.originalPropertyValue.getOrElse(flow.elements.head.code.split(" ").last)
        val apiNode     = flow.elements.last
        val domain      = getDomainFromString(literalCode)

        // Tagging the node with respective domain
        val newRuleIdToUse = ruleInfo.id + "." + domain
        ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain))
        addRuleTags(builder, apiNode, ruleInfo, ruleCache, Some(newRuleIdToUse))
        storeForTag(builder, apiNode, ruleCache)(Constants.apiUrl + newRuleIdToUse, domain)

        if (!domains.contains(domain)) {
          domains += domain
        }
      })
    }
    domains.toList
  }

}
