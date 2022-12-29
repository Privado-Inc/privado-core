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

package ai.privado.languageEngine.java.tagger.sink

import ai.privado.cache.DatabaseDetailsCache
import ai.privado.languageEngine.java.feeder.StorageInheritRule
import ai.privado.model.RuleInfo
import ai.privado.tagger.PrivadoSimplePass
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

class CustomInheritTagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[RuleInfo] = StorageInheritRule.rules.toArray
  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val typeDeclNode = cpg.typeDecl
      .filter(
        _.inheritsFromTypeFullName
          .map(inheritsFrom => inheritsFrom.matches(ruleInfo.patterns.head))
          .foldLeft(false)((a, b) => a || b)
      )
      .l
    if (typeDeclNode.nonEmpty) {
      typeDeclNode.fullName.dedup.foreach(typeDeclName => {
        val callNodes = cpg.call.methodFullName(typeDeclName + ".*" + ruleInfo.patterns(1)).l

        if (
          callNodes != null & ruleInfo.id
            .matches("Sinks.Database.JPA.*|Storages.MongoDB.SpringFramework.*|Storages.SpringFramework.Jooq.*")
        ) {
          val databaseDetails = DatabaseDetailsCache.getDatabaseDetails(ruleInfo.id)
          logger.debug(s"Rule id: ${ruleInfo.id}, DB details ${databaseDetails}")
          if (databaseDetails.isDefined) {
            logger.debug("adding database details")
            callNodes.foreach(sink => addDatabaseDetailTags(builder, sink, databaseDetails.get))
          }
        }
        callNodes.foreach(callNode => addRuleTags(builder, callNode, ruleInfo))
      })
    }
  }
}
