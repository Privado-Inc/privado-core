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
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

class CollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  protected val methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  protected val classUrlMap: mutable.HashMap[Long, String]  = mutable.HashMap[Long, String]()
  private val logger                                        = LoggerFactory.getLogger(this.getClass)

  def getIngressUrls(): List[String] = {
    CollectionUtility.getCollectionUrls(cpg, methodUrlMap, classUrlMap)
  }

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.annotations).toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    classUrlMap.addAll(collectAnnotatedUrlsFromClasses(ruleInfo.combinedRulePattern))
    val (methodUrls, collectionMethodsCache) = collectAnnotatedUrlsFromMethods(ruleInfo.combinedRulePattern)
    methodUrlMap.addAll(methodUrls)
    tagSources(builder, ruleInfo, collectionMethodsCache)
  }

  protected def tagSources(
    builder: DiffGraphBuilder,
    ruleInfo: RuleInfo,
    collectionMethodsCache: List[Method]
  ): Unit = {
    CollectionUtility.tagDirectSources(
      builder,
      collectionMethodsCache,
      ruleCache.getRule.sources,
      ruleInfo,
      ruleCache,
      methodUrlMap = methodUrlMap,
      classUrlMap = classUrlMap
    )
    CollectionUtility.tagDerivedSources(
      cpg,
      builder,
      collectionMethodsCache,
      ruleInfo,
      ruleCache,
      methodUrlMap = methodUrlMap,
      classUrlMap = classUrlMap
    )
  }

  private def collectAnnotatedUrlsFromMethods(combinedRulePatterns: String): (Map[Long, String], List[Method]) = {
    val methodAnnotations =
      cpg.annotation
        .name(combinedRulePatterns)
        .filter(_.method.nonEmpty)
        .whereNot(_.fullName("retrofit.*")) // annotations like retrofit2.http.POST are actually APIs
        .l
    (
      methodAnnotations.map(ma => ma.method.head.id() -> CollectionUtility.getUrlFromAnnotation(ma)).toMap,
      methodAnnotations.method.l
    )
  }

  private def collectAnnotatedUrlsFromClasses(combinedRulePatterns: String): Map[Long, String] = {
    cpg.annotation
      .name(combinedRulePatterns)
      .filter(_.typeDecl.nonEmpty)
      .whereNot(_.fullName("retrofit.*")) // annotations like retrofit2.http.POST are actually APIs
      .map(classAnnotation => {
        classAnnotation.typeDecl.head.id() -> CollectionUtility.getUrlFromAnnotation(classAnnotation)
      })
      .toMap
  }
}
