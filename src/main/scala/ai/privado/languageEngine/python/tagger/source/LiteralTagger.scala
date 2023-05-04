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
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language._

class LiteralTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
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

    private lazy val secretsLiteralCached = cpg.literal
    .where(
      _.code(
        "(.*(PASSWORD|AWS_ACCESS_KEY|refresh_token|access_token|client_secret).*)|(uri)|((xox[p|b|o|a]-[0-9]{12}-[0-9]{12}-[0-9]{12}-[a-z0-9]{32}))|(https://hooks.slack.com/services/T[a-zA-Z0-9_]{8}/B[a-zA-Z0-9_]{8}/[a-zA-Z0-9_]{24})|(sk_live_[0-9a-zA-Z]{24})|(s3.amazonaws.com)|((AKIA[0-9A-Z]{16}))|(amzn\\\\.mws\\\\.[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]\\{12)|(AIza[0-9A-Za-z\\\\-_]{35})|([s|S][e|E][c|C][r|R][e|E][t|T].*['|\\\"][0-9a-zA-Z]{32,45}['|\\\"])|(?i)(.*)(https)(.*)(s3)(.*)(pem|crt|cer)(.*)"
      )
    )
    .l

  private lazy val impactedLiteralCached =
    (generalLiteralCached ::: sqlQueryLiteralCached ::: secretsLiteralCached).dedup.l
  override def generateParts(): Array[RuleInfo] = ruleCache.getRule.sources.toArray
  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val rulePattern = ruleInfo.combinedRulePattern
    val impactedLiteral =
      impactedLiteralCached.code("(?:\"|'|`)?(" + rulePattern + ")(?:\"|'|`)?").l ::: sqlBigQueryLiteralCached
        .code("(?:\"|'|`|\"\"\")(.*\\s" + rulePattern + "\\s.*)(?:\"|'|`|\"\"\")")
        .l

    impactedLiteral.foreach(literal => {
      storeForTag(builder, literal, ruleCache)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
      addRuleTags(builder, literal, ruleInfo, ruleCache)
    })
  }
}
