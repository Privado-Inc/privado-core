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

import ai.privado.cache.{DatabaseDetailsCache, RuleCache}
import ai.privado.model.{Constants, DatabaseDetails, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.CollectionHasAsScala

class RegularSinkTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  val cacheCall: List[Call] = cpg.call
    .or(_.nameNot(Operators.ALL.asScala.toSeq: _*))
    .whereNot(_.method.name(".*<meta.*>$"))
    .l

  override def generateParts(): Array[RuleInfo] = {
    ruleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.REGULAR))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val sinks = cacheCall.methodFullName("(pkg.){0,1}(" + ruleInfo.combinedRulePattern + ").*").l

    if (ruleInfo.id.equals(Constants.cookieWriteRuleId)) {
      sinks.foreach(sink => {
        val cookieNameArgument = sink.argument.or(_.argumentIndex(1), _.argumentName("name")).l
        if (cookieNameArgument.nonEmpty) {
          val cookieName = (cookieNameArgument.head match {
            case node: Literal => node.code
            case node: Identifier =>
              cpg.assignment
                .where(_.argument.code(node.code).argumentIndex(1))
                .argument
                .argumentIndex(2)
                .code
                .headOption
                .getOrElse(node.code)
            case node => node.code
          }).stripPrefix("\"").stripSuffix("\"")
          val newRuleIdToUse = ruleInfo.id + "." + cookieName
          ruleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + cookieName))
          addRuleTags(builder, sink, ruleInfo, ruleCache, Some(newRuleIdToUse))
          DatabaseDetailsCache.addDatabaseDetails(
            DatabaseDetails(
              cookieName,
              "cookie",
              ruleInfo.domains.mkString(" "),
              "Write",
              sink.file.name.headOption.getOrElse("")
            ),
            newRuleIdToUse
          )
        } else {
          addRuleTags(builder, sink, ruleInfo, ruleCache)
        }
      })
    } else
      sinks.foreach(sink => addRuleTags(builder, sink, ruleInfo, ruleCache))
  }
}
