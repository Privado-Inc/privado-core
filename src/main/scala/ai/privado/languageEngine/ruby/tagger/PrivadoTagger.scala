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

package ai.privado.languageEngine.ruby.tagger

import ai.privado.cache.{DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor, TimeMetric}
import ai.privado.languageEngine.ruby.tagger.collection.CollectionTagger
import ai.privado.languageEngine.ruby.config.RubyDBConfigTagger
import ai.privado.languageEngine.ruby.tagger.source.{IdentifierDerivedTagger, IdentifierTagger}
import ai.privado.languageEngine.ruby.feeder.{LeakageRule, StorageInheritRule}
import ai.privado.languageEngine.ruby.tagger.sink.{APITagger, InheritMethodTagger, LeakageTagger, RegularSinkTagger}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.source.{LiteralTagger, SqlQueryTagger}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.util.Calendar

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache
  ): Traversal[Tag] = {
    logger.info("Starting tagging")
    new LiteralTagger(cpg, ruleCache).createAndApply()
    new IdentifierTagger(cpg, ruleCache).createAndApply()
    new SqlQueryTagger(cpg, ruleCache).createAndApply()
    new IdentifierDerivedTagger(cpg, ruleCache).createAndApply()
    new RegularSinkTagger(cpg, ruleCache).createAndApply()
    new APITagger(cpg, ruleCache, privadoInput = privadoInputConfig).createAndApply()
    new CollectionTagger(cpg, ruleCache).createAndApply()
    new RubyDBConfigTagger(cpg).createAndApply()
    if (!ScanProcessor.config.ignoreInternalRules) {
      StorageInheritRule.rules.foreach(ruleCache.setRuleInfo)
      new InheritMethodTagger(cpg, ruleCache).createAndApply()

      LeakageRule.rules.foreach(ruleCache.setRuleInfo)
      new LeakageTagger(cpg, ruleCache).createAndApply()
    }
    logger.info("Done with tagging")
    cpg.tag
  }

}
