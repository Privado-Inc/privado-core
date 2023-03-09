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

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.languageEngine.java.language._
import ai.privado.utility.Utilities
import ai.privado.utility.Utilities.{addRuleTags, getDomainFromString, storeForTag}
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, TypeDecl}
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class FeignAPI(cpg: Cpg) {

  implicit val resolver: ICallResolver = NoResolve
  val FEIGN_CLIENT                     = "FeignClient"
  val REQUEST_LINE                     = "RequestLine"

  /** For all the feign API where we get domain like object, that will be tagged as a API sinks, rest feign API calls
    * will be returned back
    *
    * @param builder
    * @param ruleInfo
    * @param httpSources
    * @return
    */
  def tagFeignAPIWithDomainAndReturnWithoutDomainAPISinks(
    builder: DiffGraphBuilder,
    ruleInfo: RuleInfo,
    httpSources: List[AstNode]
  ): List[Call] = {
    val (typeDeclWithoutUrl, typeDeclWithUrl) = getTypeDeclUsingFeignClient
    val feignRequestLineTypeDecl              = getFeignClientTypeDeclUsingRequestLine
    val (feingAPIBeanTypeDecl, beanUrl)       = getFeignClientTypeDeclUsingBean(httpSources)

    val feignClientTypeDeclWithUrl    = mutable.HashMap[String, String]()
    val feignClientTypeDeclWithoutUrl = mutable.Set[TypeDecl]()
    feignClientTypeDeclWithoutUrl.addAll(typeDeclWithoutUrl)
    feignClientTypeDeclWithoutUrl.addAll(feignRequestLineTypeDecl)

    feignClientTypeDeclWithUrl.addAll(typeDeclWithUrl)
    if (beanUrl.isEmpty)
      feignClientTypeDeclWithoutUrl.addAll(feingAPIBeanTypeDecl)
    else if (feingAPIBeanTypeDecl.nonEmpty)
      feignClientTypeDeclWithUrl.addOne(feingAPIBeanTypeDecl.head.fullName, beanUrl)
    tagFeignCallWhichHaveUrl(builder, ruleInfo, feignClientTypeDeclWithUrl)
    val feignAPISinks = feignClientTypeDeclWithoutUrl
      .filterNot(item => feignClientTypeDeclWithUrl.contains(item.fullName))
      .method
      .callIn
      .l
    feignAPISinks
  }

  /** Return the TypeDecl where Feign client annotation is used, ex below - PersonClient
    *
    * FeignClient(url = "http://localhost:8080")
    *
    * public interface PersonClient {
    *
    * RequestMapping(method \= RequestMethod.GET, value = "/persons")
    *
    * Resources<Person> getPersons();
    *
    * }
    */
  private def getTypeDeclUsingFeignClient = {
    implicit val resolver: ICallResolver = NoResolve
    val typeDeclWithoutUrl               = ListBuffer[TypeDecl]()
    val typeDeclWithUrl                  = mutable.HashMap[String, String]()
    cpg.typeDecl
      .where(_.annotation.name(FEIGN_CLIENT))
      .foreach(typeDecl => {
        val classAnnotations = typeDecl.annotation.name(FEIGN_CLIENT).l
        val annotationCode = classAnnotations.code.headOption
          .getOrElse("")
        // Logic to exact the value present in `url = "value"`
        val urlParameterPattern = ".*url\\s{0,3}=\\s{0,3}(\".*\").*(,)?".r
        val apiLiteral = annotationCode match {
          case urlParameterPattern(urlParameter) =>
            urlParameter
          case _ => ""
        }
        if (apiLiteral.isEmpty)
          typeDeclWithoutUrl.append(typeDecl)
        else
          typeDeclWithUrl.addOne(typeDecl.fullName, apiLiteral)
      })
    (typeDeclWithoutUrl.l, typeDeclWithUrl)
  }

  /** Returns Type decl node which holds probable API sinks which uses RequestLine annotation
    */
  private def getFeignClientTypeDeclUsingRequestLine: List[TypeDecl] = {
    cpg.method
      .where(_.annotation.name(REQUEST_LINE))
      .typeDecl
      .l
  }

  /** Returns typeDecl node and url which are probable API sinks when used with Bean configuration
    */
  private def getFeignClientTypeDeclUsingBean(httpSources: List[AstNode]): (List[TypeDecl], String) = {
    val feignTargetCalls = cpg.method
      .where(_.annotation.name("Bean"))
      .ast
      .isCall
      .name("target")
      .or(_.where(_.methodFullName(".*(?i)feign.*")), _.ast.isIdentifier.name("Feign"))
      .l
    val targetArguments = feignTargetCalls.argument
      .whereNot(_.argumentIndex(0))
      .l
    if (targetArguments.nonEmpty) {
      implicit val engineContext: EngineContext =
        EngineContext(semantics = Utilities.getDefaultSemantics, config = EngineConfig(4))
      val feignFlows = feignTargetCalls.reachableByFlows(httpSources).l
      val firstArgument =
        targetArguments.where(_.argumentIndex(1)).code.headOption.getOrElse("").split(".class").headOption.getOrElse("")
      val apiLiteral = {
        if (feignFlows.isEmpty)
          ""
        else
          feignFlows.head.elements.head.originalPropertyValue.getOrElse(feignFlows.head.elements.head.code)
      }
      (cpg.typeDecl.name(firstArgument).l, apiLiteral)
    } else
      (List[TypeDecl](), "")
  }

  /** Tag all the feign api calls which have some url like thing associated with them
    * @param builder
    * @param ruleInfo
    * @param typeDeclsWithUrl
    */
  private def tagFeignCallWhichHaveUrl(
    builder: DiffGraphBuilder,
    ruleInfo: RuleInfo,
    typeDeclsWithUrl: mutable.HashMap[String, String]
  ): Unit = {
    typeDeclsWithUrl.foreach(entrySet => {
      val typeDecls = cpg.typeDecl.fullName(entrySet._1).l
      if (typeDecls.nonEmpty) {
        val typeDecl   = typeDecls.head
        var apiLiteral = entrySet._2.stripPrefix("\"").stripSuffix("\"")
        val apiCalls   = typeDecl.method.callIn.l
        if (ruleInfo.id.equals(Constants.internalAPIRuleId)) {
          if (apiLiteral.matches(ruleInfo.combinedRulePattern)) {
            apiCalls.foreach(apiNode => {
              addRuleTags(builder, apiNode, ruleInfo)
              storeForTag(builder, apiNode)(Constants.apiUrl + ruleInfo.id, apiLiteral)
            })
          }
        } else if (apiLiteral.startsWith("${") || apiLiteral.matches(ruleInfo.combinedRulePattern)) {
          if (apiLiteral.startsWith("${"))
            apiLiteral = apiLiteral.stripPrefix("${").stripSuffix("}").replaceAll("\\.", "_")
          apiCalls.foreach(apiNode => {
            val domain         = getDomainFromString(apiLiteral)
            val newRuleIdToUse = ruleInfo.id + "." + domain
            RuleCache.setRuleInfo(ruleInfo.copy(id = newRuleIdToUse, name = ruleInfo.name + " " + domain))
            addRuleTags(builder, apiNode, ruleInfo, Some(newRuleIdToUse))
            storeForTag(builder, apiNode)(Constants.apiUrl + newRuleIdToUse, apiLiteral)
          })
        } else {
          apiCalls.foreach(apiNode => {
            addRuleTags(builder, apiNode, ruleInfo)
            storeForTag(builder, apiNode)(Constants.apiUrl + ruleInfo.id, Constants.API)
          })
        }
      }
    })
  }

}
