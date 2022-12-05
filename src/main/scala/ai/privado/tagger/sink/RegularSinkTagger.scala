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

import ai.privado.cache.DatabaseDetailsCache
import ai.privado.tagger.PrivadoSimplePass
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

import scala.jdk.CollectionConverters.CollectionHasAsScala

class RegularSinkTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {
  val cacheCall: List[Call] = cpg.call.or(_.nameNot(Operators.ALL.asScala.toSeq: _*)).l

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    val sinks = cacheCall.methodFullName(ruleInfo.patterns.head).l
    if (sinks != null & ruleInfo.id.matches("Storages.SpringFramework.Jdbc.*")) {
      val databaseDetails = DatabaseDetailsCache.getDatabaseDetails(ruleInfo.id)
      if (databaseDetails.isDefined) {
        sinks.foreach(sink => addDatabaseDetailTags(builder, sink, databaseDetails.get))
      }
    }
    sinks.foreach(sink => addRuleTags(builder, sink, ruleInfo))
  }
}
