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
import ai.privado.languageEngine.java.language.{NodeStarters, StepsForProperty}
import ai.privado.model.{Constants, NodeType, RuleInfo}
import ai.privado.tagger.sink.APITagger
import ai.privado.tagger.utility.APITaggerUtility.sinkTagger
import ai.privado.utility.Utilities.{addRuleTags, getDomainFromString, getDomainFromTemplates, storeForTag}
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable.HashMap

class JSAPITagger(cpg: Cpg) extends APITagger(cpg) {

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    super.runOnPart(builder, ruleInfo)
    // Identification of script tag with pixel code <Script src="https://c.amazon-adsystem.com/aax2/apstag.js" strategy="lazyOnload" />
    // Tag the respective templateDom node as API sink
    val scriptTags =
      cpg.templateDom.name("JSXElement").code("(?i)[\\\"]*<script.*" + ruleInfo.combinedRulePattern + ".*").l
    scriptTags.foreach(scriptTag => {
      var newRuleIdToUse = ruleInfo.id
      if (ruleInfo.id.equals(Constants.internalAPIRuleId))
        addRuleTags(builder, scriptTag, ruleInfo)
      else {
        val domain = getDomainFromTemplates(scriptTag.code)
        newRuleIdToUse = ruleInfo.id + "." + domain
        RuleCache.setRuleInfo(
          ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain, domains = List(domain).toArray)
        )
        addRuleTags(builder, scriptTag, ruleInfo, Some(newRuleIdToUse))
      }
      storeForTag(builder, scriptTag)(Constants.apiUrl + newRuleIdToUse, scriptTag.code)
    })
  }
}
