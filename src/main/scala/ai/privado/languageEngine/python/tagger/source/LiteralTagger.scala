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

package ai.privado.languageEngine.python.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.{InternalTag, RuleInfo}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

class LiteralTagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  // Step 1.2
  // val literals = cpg.literal.code("\"(" + ruleInfo.patterns.head + ")\"").whereNot(_.code(".*\\s.*")).l
  private lazy val generalLiteralCached = cpg.literal
    .whereNot(_.code(".*\\s.*"))
    .where(_.inCall.name("(?:add|get|put|pop).*"))
    .l

  private lazy val sqlQueryLiteralCached = cpg.literal
    .whereNot(_.code(".*\\s.*"))
    .where(_.inCall.name("(?:sql|query|select|find|execute|hasattr)"))
    .l

  private lazy val sqlBigQueryLiteralCached = cpg.literal
    .where(_.code(".*(?i)(CREATE|DELETE|ALTER|INSERT|SELECT|UPDATE).*"))
    .where(_.inCall.name(Operators.assignment))
    .l

  private lazy val impactedLiteralCached =
    (generalLiteralCached ::: sqlQueryLiteralCached).dedup.l
  override def generateParts(): Array[RuleInfo] = RuleCache.getRule.sources.toArray
  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val rulePattern = ruleInfo.combinedRulePattern
    val impactedLiteral =
      impactedLiteralCached.code("(?:\"|'|`)(" + rulePattern + ")(?:\"|'|`)").l ::: sqlBigQueryLiteralCached
        .code("(?:\"|'|`|\"\"\")(.*\\s" + rulePattern + "\\s.*)(?:\"|'|`|\"\"\")")
        .l

    impactedLiteral.foreach(literal => {
      storeForTag(builder, literal)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
      addRuleTags(builder, literal, ruleInfo)
    })
  }
}
