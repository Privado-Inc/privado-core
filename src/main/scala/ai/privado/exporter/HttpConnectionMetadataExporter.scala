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

package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.semantic.Language.*
import ai.privado.languageEngine.java.language.*
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory
import ai.privado.model.{Constants, Language}
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility

class HttpConnectionMetadataExporter(cpg: Cpg, ruleCache: RuleCache) {

  private val logger                    = LoggerFactory.getLogger(getClass)
  private val FEIGN_CLIENT              = "FeignClient"
  private val SPRING_ANNOTATION_ID      = "Collections.Annotation.Spring"
  private val STRING_START_WITH_SLASH   = "/.{2,}"
  private val STRING_CONTAINS_TWO_SLASH = ".*/.*/.*"
  private val SPRING_APPLICATION_BASE_PATH =
    "(?i)(server[.]servlet[.]context-path|server[.]servlet[.]contextPath)|(spring[.]application[.]name)"
  private val URL_PATH_WITH_VARIABLE_SYMBOLS                       = "^(?=.*/)[${}/\"'a-zA-Z0-9:.,%?_=]+"
  private val ALPHABET                                             = "[a-zA-Z]"
  private val STRING_WITH_CONSECUTIVE_DOTS_OR_DOT_SLASH_OR_NEWLINE = "(?s).*(\\.\\.|\\./|\n).*"
  private val ESCAPE_STRING_SLASHES                                = "(\\\")"
  //  Regex to eliminate pattern ending with file suffix
  //  Demo: https://regex101.com/r/ojV93D/1
  private val FILE_SUFFIX_REGEX_PATTERN = ".*[.][a-z]{2,5}(\\\")?$"
  private val COMMON_FALSE_POSITIVE_EGRESS_PATTERN =
    ".*(BEGIN PRIVATE KEY|sha512|googleapis|sha1|amazonaws|</div>|</p>|<img|<class|require\\().*"

  private val SLASH_SYMBOL          = "/"
  private val FORMAT_STRING_SYMBOLS = "[{}]"

  private val LAMBDA_SERVERLESS_BASE_PATH = "service"
  private val LAMBDA_SERVERLESS_FILE_NAME = ".*serverless.yml"

  def getLiteralsFromLanguageFiles: List[String] = {
    val egressLiterals = cpg
      .literal(URL_PATH_WITH_VARIABLE_SYMBOLS)
      .filter(node => ALPHABET.r.findFirstIn(node.code).isDefined)
      .filter(!_.code.matches(STRING_WITH_CONSECUTIVE_DOTS_OR_DOT_SLASH_OR_NEWLINE))
      .inCall
      .where(_.and(_.nameNot("require"), _.nameNot("import")))
      .map(node =>
        node.argument
          .map(arg => {
            if arg.isLiteral then arg.code // collect literal
            // const loginPath = "api/v1" + "/login" --- Addition Case(javascript, python, java)
            // const signupPath = `api/v1/${signup}` --- Format String Case for javascript(similar applicable for python, java)
            else if node.name.equals("<operator>.addition") || node.name.equals("<operator>.formatString") then
              arg.code.replaceAll(FORMAT_STRING_SYMBOLS, "")
            // const loginPath = "api/v1/login" --- Assignment Case(similar applicable for python, java)
            else if node.name.equals("<operator>.assignment") && arg.isLiteral then arg.code
            else ""
          })
          .mkString("")
      )
      .filter(_.nonEmpty)
      .map(value => value.replaceAll(ESCAPE_STRING_SLASHES, ""))
      .dedup
      .l

    egressLiterals
  }

  def getEgressUrlsFromCodeFiles: List[String] = {
    var egressUrls = List[String]()

    egressUrls = egressUrls.concat(
      cpg.property
        .filterNot(_.value.matches(FILE_SUFFIX_REGEX_PATTERN))
        .filterNot(_.value.matches(COMMON_FALSE_POSITIVE_EGRESS_PATTERN))
        .or(_.value(STRING_START_WITH_SLASH), _.value(STRING_CONTAINS_TWO_SLASH))
        .value
        .dedup
        .l
    )
    /* We have verified literals for these languages, so we need to analyze other languages before broadening the rule.
       It can happen that literals may come from imports as well, which was the case for JavaScript, and we handled it.
       Therefore, we might need to do a few things specific to each language. */
    if (
      AppCache.repoLanguage == Language.JAVA || AppCache.repoLanguage == Language.PYTHON || AppCache.repoLanguage == Language.JAVASCRIPT
    ) {
      egressUrls = egressUrls.concat(getLiteralsFromLanguageFiles)
    }

    egressUrls.dedup.l
  }

  def getEgressUrls: List[String] = {
    var egressUrls = List[String]()

    egressUrls = egressUrls.concat(
      cpg.property.or(_.value(STRING_START_WITH_SLASH), _.value(STRING_CONTAINS_TWO_SLASH)).value.dedup.l
    )

    egressUrls = egressUrls.concat(addUrlFromFeignClient())
    egressUrls.dedup.l
  }

  def getEndPointBasePath: List[String] = {
    var basePaths = List[String]()
    if (AppCache.repoLanguage.id == Language.JAVA.id) {
      basePaths = basePaths.concat(cpg.property.name(SPRING_APPLICATION_BASE_PATH).value.dedup.l)
    }
    basePaths = basePaths.concat(
      cpg.property.where(_.file.name(LAMBDA_SERVERLESS_FILE_NAME)).name(LAMBDA_SERVERLESS_BASE_PATH).value.dedup.l
    )
    basePaths.dedup.l
  }

  private def addUrlFromFeignClient(): List[String] = {

    var egressUrls = List[String]()
    try {
      val filesHavingFeignClient = cpg.annotation.name(FEIGN_CLIENT).file.name.l

      val ruleInfo = ruleCache.getRule.collections
        .filter(_.catLevelTwo == Constants.annotations)
        .filter(_.id == SPRING_ANNOTATION_ID)
        .headOption

      if (ruleInfo.isEmpty) return egressUrls

      val combinedRulePatterns = ruleInfo.get.combinedRulePattern

//       filters these annotation to include only those found in files that contain a FeignClient,
//       producing a list of matched annotations
      val matchedAnnotations = cpg.method.annotation
        .name(combinedRulePatterns)
        .filter(x => filesHavingFeignClient.contains(x.file.name.head))
        .l

      for (matchedAnnotation <- matchedAnnotations) {
        val classLevelAnnotation = matchedAnnotation.method.typeDecl.annotation.name(FEIGN_CLIENT).headOption
        if (classLevelAnnotation.isDefined) {
          egressUrls = egressUrls :+ CollectionUtility
//            In case Feign client name of service is denoted by parameter "name"
            .getUrlFromAnnotation(classLevelAnnotation.get, List("name"))
            .stripSuffix("/") + SLASH_SYMBOL + CollectionUtility
            .getUrlFromAnnotation(matchedAnnotation)
            .strip()
            .stripPrefix("/")
            .stripSuffix("/")
        } else {
          egressUrls = egressUrls :+ CollectionUtility.getUrlFromAnnotation(matchedAnnotation)
        }
      }
    } catch {
      case e: Exception =>
        logger.error("Error while adding URL from FeignClient annotation", e)
    }
    egressUrls
  }
}
