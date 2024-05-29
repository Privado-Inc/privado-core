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

package ai.privado.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, DEDRuleInfo, DEDVariable, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Identifier, Member}
import io.shiftleft.semanticcpg.language.*

class DEDTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[DEDRuleInfo](cpg) {
  override def generateParts(): Array[DEDRuleInfo] = ruleCache.getRule.dedRules.toArray

  override def runOnPart(builder: DiffGraphBuilder, dedRuleInfo: DEDRuleInfo): Unit = {
    val dedFilePath = dedRuleInfo.filePath.trim

    // Identifiers
    val filteredIdentifiers = cpg.identifier.filter(p => p.file.nameExact(dedFilePath).nonEmpty).l

    // Members
    val filteredMembers = cpg.member.filter(p => p.file.nameExact(dedFilePath).nonEmpty).l

    //  FieldAccess
    val filteredFieldAccessIdentifier = cpg.fieldAccess.filter(p => p.file.nameExact(dedFilePath).nonEmpty).l

    //  Parameters
    val filteredParameter = cpg.parameter.filter(p => p.file.nameExact(dedFilePath).nonEmpty).l

    //  Locals
    val filteredLocals = cpg.local.filter(p => p.file.nameExact(dedFilePath).nonEmpty).l

    //  TODO: SqlColumnNode

    def getMatchesNodes(v: DEDVariable): List[AstNode] = {
      filteredIdentifiers.filter(_.name == v.name)
        ++ filteredMembers.filter(_.name == v.name)
        ++ filteredFieldAccessIdentifier.filter(_.fieldIdentifier.canonicalName(v.name).nonEmpty).isCall.l
        ++ filteredLocals.filter(_.name == v.name)
        ++ filteredParameter.filter(_.name == v.name)
    }

    dedRuleInfo.classificationData.foreach { dedData =>
      val id           = dedData.id
      val variables    = dedData.variables
      val someRuleInfo = ruleCache.getRuleInfo(id)

      if (id.contentEquals(Constants.disabledByDEDId)) {
        variables.foreach { v =>
          val matchedNodes: List[AstNode] = getMatchesNodes(v)
          matchedNodes.foreach { mIdentifier =>
            storeForTag(builder, mIdentifier, ruleCache)(InternalTag.TAGGING_DISABLED_BY_DED.toString)
          }
        }
      }

      someRuleInfo match
        case Some(ruleInfo): Some[RuleInfo] =>
          variables.foreach { v =>
            val matchedNodes: List[AstNode] = getMatchesNodes(v)

            matchedNodes.foreach { mIdentifier =>
              storeForTag(builder, mIdentifier, ruleCache)(InternalTag.TAGGED_BY_DED.toString)
              storeForTag(builder, mIdentifier, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
              addRuleTags(builder, mIdentifier, ruleInfo, ruleCache)
            }
          }
        case None =>
          None
    }
  }
}
