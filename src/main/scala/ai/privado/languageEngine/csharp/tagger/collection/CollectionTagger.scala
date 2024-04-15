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
  enum NodeType:
    case Method, Class

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

  private def getUrlFromAnnotationsCode(
    annotation: Annotation,
    nodeType: NodeType,
    combinedRulePatterns: String
  ): String = {
    // If we have [controller] in annotation code, then replace with the class name of controller
    //
    //    [Route("api/some/[controller]")]                      <-- /api/some/email            [CASE A]
    //    class EmailController {
    //      [HttpGet("[controller]/[action]/{id}")]             <-- /api/some/email/edit/{id}  [CASE B]
    //      public IActionResult Edit(int id)
    //      {
    //        return ControllerContext.MyDisplayRouteInfo(id);
    //      }
    //    }
    //
    val controllerName = if annotation.code.contains("[controller]") then {
      nodeType match
        case NodeType.Class =>
          annotation.start.typeDecl
            .where(_.name(".*Controller"))
            .name
            .headOption
            .getOrElse("")
            .stripSuffix("Controller")
            .toLowerCase

        case NodeType.Method =>
          annotation.start.method.typeDecl
            .where(_.name(".*Controller"))
            .name
            .headOption
            .getOrElse("")
            .stripSuffix("Controller")
            .toLowerCase
    } else {
      ""
    }

    // we get the action name. This can only be the method
    val actionName = annotation.start.method.name.headOption.getOrElse("").toLowerCase

    // Sometimes, we will get something like the following:
    //
    //    [Route("api/[controller]/[action]")]
    //    public class SomeController : Controller {
    //      [HttpGet]                                        <-- /api/some/copy           [CASE Ca]
    //      public IActionResult Copy(){...}
    //
    //      [HttpGet("{id}")]                                <-- /api/some/paste/{id}     [CASE D]
    //      public IActionResult Paste(int id){...}
    //    }
    //
    //    [Route("api/[controller]")]
    //    public class OtherController : Controller {
    //      [HttpPost]                                        <-- /api/other/cut          [CASE Cb]
    //      public IActionResult Cut(){...}
    //    }

    // We now try to build a URL under various combinations of annotations for Methods and Classes by doing token replacement. We try to cover most cases in the following guideline:
    // https://learn.microsoft.com/en-us/aspnet/core/mvc/controllers/routing?view=aspnetcore-8.0#token-replacement-in-route-templates-controller-action-area
    val url = nodeType match

      case NodeType.Class =>
        annotation.code
          .replaceAll(".*\"(.*?)\".*", "/$1")
          .replace("[controller]", controllerName)
          .replace("[action]", actionName)

      case NodeType.Method =>
        // we are interested in cases where the method doesn't have action defined, but its containing type has
        if annotation.start.method.typeDecl.annotation
            .name(combinedRulePatterns)
            .code
            .headOption
            .getOrElse("")
            .matches(".*\\[action\\].*") && !annotation.code.matches(".*\\[action\\].*")
        then {
          if annotation.code.matches(".*\"(.*?)\".*") then {
            s"$actionName" + annotation.code.replaceAll(
              ".*\"(.*?)\".*",
              "/$1"
            ) // CASE D: we append action name for case like [HttpGet] where there is no argument
          } else if !annotation.code.matches(".*\"(.*?)\".*") then {
            s"$actionName" // CASE Ca: No brackets in annotation, means we don't need to append
          } else {
            "" // we should not be here
          }
        } else if annotation.start.method.typeDecl.annotation
            .name(combinedRulePatterns)
            .code
            .headOption
            .getOrElse("")
            .matches(".*\\[controller\\].*") && !annotation.code.matches(".*\\[action\\].*") && !annotation.code
            .matches(".*\"(.*?)\".*")
        then {
          s"/$actionName" // Case Cb: No brackets in annotation, class has [controller], means we don't need to append
        } else {
          annotation.code // for all other cases (eg. CASE B)
            .replaceAll(".*\"(.*?)\".*", "/$1")
            .replace("[controller]", controllerName)
            .replace("[action]", actionName)
        }
    url
  }

  private def collectAnnotatedUrlsFromMethods(combinedRulePatterns: String): (Map[Long, String], List[Method]) = {
    val methodAnnotations =
      cpg.annotation
        .name(combinedRulePatterns)
        .filter(_.method.nonEmpty)
        .l
    (
      methodAnnotations
        .map(ma => ma.method.head.id() -> getUrlFromAnnotationsCode(ma, NodeType.Method, combinedRulePatterns))
        .toMap,
      methodAnnotations.method.l
    )
  }

  private def collectAnnotatedUrlsFromClasses(combinedRulePatterns: String): Map[Long, String] = {
    cpg.annotation
      .name(combinedRulePatterns)
      .filter(_.typeDecl.nonEmpty)
      .map(classAnnotation => {
        classAnnotation.typeDecl.head
          .id() -> getUrlFromAnnotationsCode(classAnnotation, NodeType.Class, combinedRulePatterns)
      })
      .toMap
  }
}
