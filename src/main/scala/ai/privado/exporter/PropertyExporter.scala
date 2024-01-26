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

class PropertyExporter(cpg: Cpg, ruleCache: RuleCache) {

  private val logger = LoggerFactory.getLogger(getClass)

  def getPropertyUrls = {
    var propertyUrls = List[String]()
    val apiRules = ruleCache.getAllRuleInfo
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toList

    apiRules.foreach { ruleInfo =>
      propertyUrls = propertyUrls.concat(cpg.property.filter(p => p.value matches ruleInfo.combinedRulePattern).value.l)
    }
    for (ruleInfo <- ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.annotations).toArray) {
      // 1. Get all the files where FeignClient is used.
      // 2. Get url from XXXMapping and add it to list (filter over #1)

      val filesHavingFeignClient = cpg.annotation.name("FeignClient").file.name.l
      val combinedRulePatterns   = ruleInfo.combinedRulePattern

      cpg.annotation
        .name(combinedRulePatterns)
        .filter(x => filesHavingFeignClient.contains(x.file.name.head))
        .map(matchedAnnotation => {
          propertyUrls :+ CollectionUtility.getUrlFromAnnotation(matchedAnnotation)
          matchedAnnotation
        })

    }
    propertyUrls.dedup.l
  }
}
