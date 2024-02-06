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

import ai.privado.cache.RuleCache
import ai.privado.semantic.Language.*
import ai.privado.languageEngine.java.language.*
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory
import ai.privado.model.{Constants, NodeType}
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility

class EgressExporter(cpg: Cpg, ruleCache: RuleCache) {

  private val logger                    = LoggerFactory.getLogger(getClass)
  private val FEIGN_CLIENT              = "FeignClient"
  private val SPRING_ANNOTATION_ID      = "Collections.Annotation.Spring"
  private val STRING_START_WITH_SLASH   = "/.*"
  private val STRING_CONTAINS_TWO_SLASH = ".*/.*/.*"

  def getEgressUrls = {
    var egressUrls = List[String]()

    egressUrls = egressUrls.concat(
      cpg.property.or(_.value(STRING_START_WITH_SLASH), _.value(STRING_CONTAINS_TWO_SLASH)).value.dedup.l
    )

    egressUrls = egressUrls.concat(addUrlFromFeignClient())
    egressUrls.dedup.l
  }

  private def addUrlFromFeignClient(): List[String] = {

    var egressUrls = List[String]()
    try {
      val filesHavingFeignClient = cpg.annotation.name(FEIGN_CLIENT).file.name.l

      val ruleInfo = ruleCache.getRule.collections
        .filter(_.catLevelTwo == Constants.annotations)
        .filter(_.id == SPRING_ANNOTATION_ID)
        .head

      val combinedRulePatterns = ruleInfo.combinedRulePattern
      val matchedAnnotations =
        cpg.annotation.name(combinedRulePatterns).filter(x => filesHavingFeignClient.contains(x.file.name.head)).l

      for (matchedAnnotation <- matchedAnnotations) {
        egressUrls = egressUrls :+ CollectionUtility.getUrlFromAnnotation(matchedAnnotation)
      }
    } catch {
      case e: Exception => {
        logger.error("Error while adding URL from FeignClient annotation", e)
      }
    }
    egressUrls
  }
}
