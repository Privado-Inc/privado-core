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

package ai.privado.tagger.sink

import ai.privado.cache.{DatabaseDetailsCache, RuleCache}
import ai.privado.model.{FilterProperty, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.*
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.CollectionHasAsScala

class RegularSinkTagger(cpg: Cpg, ruleCache: RuleCache, databaseDetailsCache: DatabaseDetailsCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  val cacheCall: List[Call] = cpg.call.or(_.nameNot(Operators.ALL.asScala.toSeq: _*)).l

  override def generateParts(): Array[RuleInfo] = {
    ruleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.REGULAR))
      .toArray
  }
  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val sinks = ruleInfo.filterProperty match
      case FilterProperty.CODE => cacheCall.code(s"${ruleInfo.combinedRulePattern}").l
      // Default is METHOD_FULL_NAME
      case _ =>
        cacheCall
          .or(
            _.methodFullName(ruleInfo.combinedRulePattern),
            _.filter(_.dynamicTypeHintFullName.exists(_.matches(ruleInfo.combinedRulePattern)))
          )
          .l

    if (sinks != null & ruleInfo.id.matches("Storages.SpringFramework.Jdbc.*")) {
      val databaseDetails = databaseDetailsCache.getDatabaseDetails(ruleInfo.id)
      println("JDBC Tags match here.....")
      if (databaseDetails.isDefined) {
        println("database details defined for JDBC")
        println(databaseDetails)
        sinks.foreach(sink => addDatabaseDetailTags(builder, sink, databaseDetails.get, ruleCache))
      }
    }

    sinks.foreach(sink => {
      addRuleTags(builder, sink, ruleInfo, ruleCache)
    })
  }
}
