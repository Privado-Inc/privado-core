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
 */

package ai.privado.languageEngine.javascript.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.{DEDRuleInfo, InternalTag, RuleInfo, Constants}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class DEDTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[DEDRuleInfo](cpg) {
  val identifiers = cpg.identifier.l

  override def generateParts(): Array[DEDRuleInfo] = ruleCache.getRule.dedRules.toArray
  override def runOnPart(builder: DiffGraphBuilder, dedRuleInfo: DEDRuleInfo): Unit = {
    val identifiersInFile = cpg.identifier.filter(p => p.file.name.contains(dedRuleInfo.filePath)).l

    dedRuleInfo.classificationData.foreach((dedData) => {
      val id  = dedData.id
      val variables = dedData.variables
      val ruleInfo = ruleCache.getRuleInfo(id).getOrElse(null)
      println(id)

      if (id.contentEquals(Constants.disabledByDEDId)) {
        variables.foreach((v) => {
          val matchedNodes = identifiersInFile.filter(i => i.name.contentEquals(v.name))
          matchedNodes.foreach((mIdentifier) => {
            println(s"Inside the TAGGING_DISABLED_BY_DED ${mIdentifier.name}")
            storeForTag(builder, mIdentifier, ruleCache)(InternalTag.TAGGING_DISABLED_BY_DED.toString)
            // addRuleTags(builder, mIdentifier, ruleInfo, ruleCache)
          })
        })
      }

      if (ruleInfo.isInstanceOf[RuleInfo]) {
        variables.foreach((v) => {
          val matchedNodes = identifiersInFile.filter(i => i.name.contentEquals(v.name))
          println(v)
          matchedNodes.foreach((mIdentifier) => {
            println("____________________________")
            println(mIdentifier.name)
            println(mIdentifier.lineNumber)
            println(mIdentifier.file.name.l)

            storeForTag(builder, mIdentifier, ruleCache)(InternalTag.TAGGED_BY_DED.toString)
            storeForTag(builder, mIdentifier, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
            addRuleTags(builder, mIdentifier, ruleInfo, ruleCache)
          })
        })
      }
    })

  }
}
