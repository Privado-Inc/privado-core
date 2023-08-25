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
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.sink.APITagger
import ai.privado.utility.Utilities.{addRuleTags, getDomainFromTemplates, storeForTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language._

class JSAPITagger(cpg: Cpg, ruleCache: RuleCache) extends APITagger(cpg, ruleCache) {

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    super.runOnPart(builder, ruleInfo)
    // Identification of script tag with pixel code <Script src="https://c.amazon-adsystem.com/aax2/apstag.js" strategy="lazyOnload" />
    // Tag the respective templateDom node as API sink
    val scriptTags =
      cpg.templateDom.name(Constants.jsxElement).code("(?i)[\\\"]*<script.*" + ruleInfo.combinedRulePattern + ".*").l
    scriptTags.foreach(scriptTag => {
      var newRuleIdToUse = ruleInfo.id
      val domain         = getDomainFromTemplates(scriptTag.code)
      if (ruleInfo.id.equals(Constants.internalAPIRuleId)) addRuleTags(builder, scriptTag, ruleInfo, ruleCache)
      else {
        newRuleIdToUse = ruleInfo.id + "." + domain._2
        ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain._2))
        addRuleTags(builder, scriptTag, ruleInfo, ruleCache, Some(newRuleIdToUse))
      }
      storeForTag(builder, scriptTag, ruleCache)(Constants.apiUrl + newRuleIdToUse, domain._1)
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
}
