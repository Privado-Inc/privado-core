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

package ai.privado.languageEngine.java.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.RuleInfo
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable

class CollectionTagger(cpg: Cpg, sourceRuleInfos: List[RuleInfo]) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[RuleInfo] = RuleCache.getRule.collections.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val methodUrlMap = mutable.HashMap[Long, String]()
    val classUrlMap  = mutable.HashMap[Long, String]()

    // A cached method so that we are not computing again
    val combinedRulePatterns = ruleInfo.combinedRulePattern
    cpg.annotation
      .name(combinedRulePatterns)
      .filter(_.typeDecl.nonEmpty)
      .foreach(classAnnotation => {
        classUrlMap
          .addOne(classAnnotation.typeDecl.head.id() -> CollectionUtility.getUrlFromAnnotation(classAnnotation))
      })
    val collectionMethodsCache = cpg.annotation
      .name(combinedRulePatterns)
      .filter(_.method.nonEmpty)
      .map(matchedAnnotation => {
        methodUrlMap.addOne(
          matchedAnnotation.method.head.id() -> CollectionUtility.getUrlFromAnnotation(matchedAnnotation)
        )
        matchedAnnotation
      })
      .method
      .l

    CollectionUtility.tagDirectSources(
      builder,
      collectionMethodsCache,
      sourceRuleInfos,
      ruleInfo,
      methodUrlMap = methodUrlMap,
      classUrlMap = classUrlMap
    )
    CollectionUtility.tagDerivedSources(
      cpg,
      builder,
      collectionMethodsCache,
      ruleInfo,
      methodUrlMap = methodUrlMap,
      classUrlMap = classUrlMap
    )
  }

}
