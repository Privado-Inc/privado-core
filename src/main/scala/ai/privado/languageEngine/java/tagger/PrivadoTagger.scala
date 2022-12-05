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

package ai.privado.languageEngine.java.tagger

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.ScanProcessor
import ai.privado.languageEngine.java.feeder.StorageInheritRule
import ai.privado.languageEngine.java.tagger.collection.CollectionTagger
import ai.privado.languageEngine.java.tagger.sink.CustomInheritTagger
import ai.privado.languageEngine.java.tagger.source.IdentifierTagger
import ai.privado.model.{ConfigAndRules, NodeType}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.config.DBConfigTagger
import ai.privado.tagger.sink.{APITagger, RegularSinkTagger}
import ai.privado.tagger.source.LiteralTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.util.Calendar

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(rules: ConfigAndRules): Traversal[Tag] = {

    logger.info("Starting tagging")
    val literalTagger       = new LiteralTagger(cpg)
    val identifierTagger    = new IdentifierTagger(cpg)
    val apiTagger           = new APITagger(cpg)
    val regularSinkTagger   = new RegularSinkTagger(cpg)
    val customInheritTagger = new CustomInheritTagger(cpg)

    val sourceRules = rules.sources

    println(s"${Calendar.getInstance().getTime} - LiteralTagger invoked...")
    sourceRules.foreach(rule => {
      literalTagger.setRuleAndApply(rule)
    })
    println(s"${Calendar.getInstance().getTime} - IdentifierTagger invoked...")
    sourceRules.foreach(rule => {
      identifierTagger.setRuleAndApply(rule)
    })
    println(s"${Calendar.getInstance().getTime} - DBConfigTagger invoked...")
    new DBConfigTagger(cpg).createAndApply()
    println(s"${Calendar.getInstance().getTime} - RegularSinkTagger invoked...")
    rules.sinks
      .filter(rule => rule.nodeType.equals(NodeType.REGULAR))
      .foreach(rule => regularSinkTagger.setRuleAndApply(rule))
    println(s"${Calendar.getInstance().getTime} - APITagger invoked...")
    rules.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .foreach(rule => apiTagger.setRuleAndApply(rule))

    // Custom Rule tagging
    if (!ScanProcessor.config.ignoreInternalRules) {
      // Adding custom rule to cache
      StorageInheritRule.rules.foreach(RuleCache.setRuleInfo)
      println(s"${Calendar.getInstance().getTime} - CustomInheritTagger invoked...")
      StorageInheritRule.rules.foreach(rule => customInheritTagger.setRuleAndApply(rule))
    }

    println(s"${Calendar.getInstance().getTime} - CollectionTagger invoked...")
    val collectionTagger = new CollectionTagger(cpg, sourceRules)
    rules.collections.foreach(rule => collectionTagger.setRuleAndApply(rule))
    logger.info("Done with tagging")

    cpg.tag
  }

}
