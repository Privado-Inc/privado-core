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

package ai.privado.languageEngine.javascript.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.javascript.processor.JavascriptProcessor
import ai.privado.model.{InternalTag, RuleInfo}
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import better.files.File
import org.slf4j.LoggerFactory

import java.util.UUID
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success}

class CollectionTagger(cpg: Cpg, sourceRuleInfos: List[RuleInfo]) extends ConcurrentWriterCpgPass[RuleInfo](cpg) {
  lazy val cacheCall = cpg.call.or(_.nameNot(Operators.ALL.asScala.toSeq: _*)).l

  private val logger                            = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[RuleInfo] = RuleCache.getRule.collections.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val collectionPoints = cacheCall.methodFullName("(pkg.){0,1}(" + ruleInfo.patterns.head + ").*").l
    collectionPoints.foreach(collectionPoint => {
      val dir = File.newTemporaryDirectory()
      (dir / "collectionPoint.js").write(collectionPoint.code)
      JavascriptProcessor.getCpg(dir.toString()) match {
        case Success(cpCpg) =>
          sourceRuleInfos.foreach(sourceRule => {
            if (
              cpCpg.identifier(sourceRule.patterns.head).nonEmpty || cpCpg.fieldAccess
                .where(_.fieldIdentifier.canonicalName(sourceRule.patterns.head))
                .isCall
                .nonEmpty
            ) {
              addRuleTags(builder, collectionPoint, ruleInfo)
              storeForTag(builder, collectionPoint)(
                InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
                collectionPoint.argument(1).code
              )
              storeForTag(builder, collectionPoint)(
                InternalTag.COLLECTION_METHOD_SOURCE_RULE.toString + "_" + UUID.randomUUID().toString,
                sourceRule.id
              )
            }
          })
        case Failure(e) => // collection point cpg creation failed
          logger.debug("Exception occurred when forming collection point cpg : ", e)
      }
    })
  }
}
