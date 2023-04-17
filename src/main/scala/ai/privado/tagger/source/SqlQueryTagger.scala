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

package ai.privado.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.{InternalTag, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import ai.privado.semantic.Language._
import ai.privado.utility.Utilities.{storeForTag, addRuleTags}

class SqlQueryTagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  override def generateParts(): Array[_ <: AnyRef] = RuleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    cpg.sqlQuery
      .filter(queryNode => {
        val columns = queryNode.value.split(",").toList
        columns.map(_.matches(ruleInfo.combinedRulePattern)).foldLeft(false)(_ || _)
      })
      .foreach(queryNode => {
        storeForTag(builder, queryNode)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
        addRuleTags(builder, queryNode, ruleInfo)
      })
  }
}
