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

package ai.privado.languageEngine.csharp.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Annotation, Method}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

class CollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  protected val methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  protected val classUrlMap: mutable.HashMap[Long, String]  = mutable.HashMap[Long, String]()
  private val logger                                        = LoggerFactory.getLogger(this.getClass)

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

  private def getUrlFromAnnotationsCode(annotation: Annotation): String = {
    // If we have [controller] in annotation code, then replace with the class name of controller
    //
    //    [Route("api/some/[controller]")]                      <-- /api/some/email
    //    class EmailController {
    //      [HttpGet("[controller]/[action]/{id}")]             <-- /api/some/email/edit/{id}
    //      public IActionResult Edit(int id)
    //      {
    //        return ControllerContext.MyDisplayRouteInfo(id);
    //      }
    //    }
    //
    val controllerName = if annotation.code.contains("[controller]") then {
      cpg.typeDecl
        .where(_.annotation.codeExact(annotation.code))
        .where(_.name(".*Controller"))
        .name
        .headOption
        .getOrElse("")
        .split("Controller")
        .headOption
        .getOrElse("")
        .toLowerCase
    } else {
      ""
    }

    // Now we extract [action] seen in snippet above. Only method support for now since we encounter them most here
    // TODO: Support [action] replacement for classes as well eg Route("[controller]/[action]")
    val actionName = if annotation.code.contains("[action]") then {
      cpg.method.where(_.annotation.codeExact(annotation.code)).name.headOption.getOrElse("").toLowerCase
    } else {
      ""
    }
    annotation.code
      .replaceAll(".*\"(.*?)\".*", "/$1")
      .replace("[controller]", controllerName)
      .replace("[action]", actionName)
  }

  private def collectAnnotatedUrlsFromMethods(combinedRulePatterns: String): (Map[Long, String], List[Method]) = {
    val methodAnnotations =
      cpg.annotation
        .name(combinedRulePatterns)
        .filter(_.method.nonEmpty)
        .l
    (
      methodAnnotations.map(ma => ma.method.head.id() -> getUrlFromAnnotationsCode(ma)).toMap,
      methodAnnotations.method.l
    )
  }

  private def collectAnnotatedUrlsFromClasses(combinedRulePatterns: String): Map[Long, String] = {
    cpg.annotation
      .name(combinedRulePatterns)
      .filter(_.typeDecl.nonEmpty)
      .map(classAnnotation => {
        classAnnotation.typeDecl.head.id() -> getUrlFromAnnotationsCode(classAnnotation)
      })
      .toMap
  }
}
