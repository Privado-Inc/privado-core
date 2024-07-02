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
import ai.privado.utility.Utilities.*
import io.joern.x2cpg.utils.LinkingUtil
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.Node

object LiteralTagger {
  def tag(cpg: Cpg, ruleCache: RuleCache): Unit = {
    List(GeneralAndSqlLiteralTagger(cpg, ruleCache), BigSQLLiteralTagger(cpg, ruleCache)).foreach(_.createAndApply())
  }
}

class GeneralAndSqlLiteralTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[List[Literal]](cpg)
    with LinkingUtil
    with LiteralTagger {
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

  override def generateParts(): Array[List[Literal]] = {
    val nodes = (generalLiteralCached ::: sqlQueryLiteralCached).dedup.l
    // TODO: MAX_BATCH_SIZE  will be replaced when optimized upstream
    nodes.grouped(MAX_BATCH_SIZE).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, literals: List[Literal]): Unit = {

    ruleCache.getRule.sources.foreach(rule => {
      processRulesAndTag(builder, literals, ruleCache, s"(?:\"|'|`)(${rule.combinedRulePattern})(?:\"|'|`)", rule)
    })
  }
}

class BigSQLLiteralTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[List[Literal]](cpg)
    with LinkingUtil
    with LiteralTagger {

  override def generateParts(): Array[List[Literal]] = {
    val nodes = cpg.literal
      .where(_.code(".*(?i)(CREATE|DELETE|ALTER|INSERT|SELECT|UPDATE).*"))
      .where(_.inCall.name(Operators.assignment))
      .l
    // TODO: MAX_BATCH_SIZE  will be replaced when optimized upstream
    nodes.grouped(MAX_BATCH_SIZE).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, literals: List[Literal]): Unit = {
    ruleCache.getRule.sources.foreach(rule => {
      processRulesAndTag(
        builder,
        literals,
        ruleCache,
        s"(?:\"|'|`|\"\"\")(.*\\s${rule.combinedRulePattern}\\s.*)(?:\"|'|`|\"\"\")",
        rule
      )
    })
  }
}

trait LiteralTagger {
  def processRulesAndTag(
    builder: DiffGraphBuilder,
    literals: List[Literal],
    ruleCache: RuleCache,
    rulePattern: String,
    ruleInfo: RuleInfo
  ): Unit = {
    val impactedLiteral = literals.code(rulePattern).l

    impactedLiteral.foreach(literal => {
      storeForTag(builder, literal, ruleCache)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
      addRuleTags(builder, literal, ruleInfo, ruleCache)
    })
  }
}
