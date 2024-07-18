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

import ai.privado.cache.{AppCache, DataFlowCache, DatabaseDetailsCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.ruby.tagger.collection.CollectionTagger
import ai.privado.languageEngine.ruby.config.RubyDBConfigTagger
import ai.privado.languageEngine.ruby.feeder.{LeakageRule, StorageInheritRule}
import ai.privado.languageEngine.ruby.tagger.monolith.MonolithTagger
import ai.privado.languageEngine.ruby.tagger.sink.{
  InheritMethodTagger,
  LeakageTagger,
  RegularSinkTagger,
  RubyAPISinkTagger
}
import ai.privado.languageEngine.ruby.tagger.schema.{RubyMongoSchemaMapper, RubyMongoSchemaTagger}
import ai.privado.languageEngine.ruby.tagger.source.{
  IdentifierDerivedTagger,
  IdentifierTagger,
  RubyLiteralDerivedTagger,
  RubyLiteralTagger
}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.source.{DEDTagger, LiteralTagger, SqlQueryTagger}
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder
  ): Traversal[Tag] = {
    logger.info("Starting tagging")
    new DEDTagger(cpg, ruleCache).createAndApply()
    new LiteralTagger(cpg, ruleCache).createAndApply()
    new RubyLiteralTagger(cpg, ruleCache).createAndApply()
    new RubyLiteralDerivedTagger(cpg, ruleCache).createAndApply()
    new IdentifierTagger(cpg, ruleCache).createAndApply()
    new SqlQueryTagger(cpg, ruleCache).createAndApply()
    new IdentifierDerivedTagger(cpg, ruleCache).createAndApply()
    new RegularSinkTagger(cpg, ruleCache).createAndApply()

    RubyAPISinkTagger.applyTagger(cpg, ruleCache, privadoInputConfig, appCache, statsRecorder)

    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    appCache.ingressUrls.addAll(collectionTagger.getIngressUrls())

    new RubyDBConfigTagger(cpg, databaseDetailsCache).createAndApply()
    if (!privadoInputConfig.ignoreInternalRules) {
      StorageInheritRule.rules.foreach(ruleCache.setRuleInfo)
      new InheritMethodTagger(cpg, ruleCache).createAndApply()

      LeakageRule.rules.foreach(ruleCache.setRuleInfo)
      new LeakageTagger(cpg, ruleCache).createAndApply()
    }

    new RubyMongoSchemaTagger(cpg, ruleCache).createAndApply()
    new RubyMongoSchemaMapper(cpg, ruleCache, databaseDetailsCache).createAndApply()

    // Run monolith tagger at the end
    new MonolithTagger(cpg, ruleCache).createAndApply()
    logger.info("Done with tagging")
    cpg.tag
  }

}
