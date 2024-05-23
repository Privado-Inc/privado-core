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

package ai.privado.languageEngine.javascript.tagger

import ai.privado.cache.{AppCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.feeder.PermissionSourceRule
import ai.privado.languageEngine.javascript.config.JSDBConfigTagger
import ai.privado.languageEngine.javascript.passes.read.GraphqlQueryParserPass
import ai.privado.languageEngine.javascript.tagger.collection.CollectionTagger
import ai.privado.languageEngine.javascript.tagger.sink.{GraphqlAPITagger, JSAPITagger, RegularSinkTagger}
import ai.privado.languageEngine.javascript.tagger.source.{IdentifierTagger, LiteralTagger, DEDTagger}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.collection.WebFormsCollectionTagger
import ai.privado.tagger.source.{AndroidXmlPermissionTagger, SqlQueryTagger}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal
import ai.privado.utility.Utilities.ingressUrls

import java.util.Calendar

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    appCache: AppCache
  ): Traversal[Tag] = {

    logger.info("Starting tagging")

    new DEDTagger(cpg, ruleCache).createAndApply()

    new LiteralTagger(cpg, ruleCache).createAndApply()

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new RegularSinkTagger(cpg, ruleCache).createAndApply()

    new JSAPITagger(cpg, ruleCache, privadoInput = privadoInputConfig, appCache = appCache).createAndApply()

    new GraphqlAPITagger(cpg, ruleCache).createAndApply()

    new GraphqlQueryParserPass(cpg, ruleCache, taggerCache).createAndApply()

    new JSDBConfigTagger(cpg).createAndApply()

    new WebFormsCollectionTagger(cpg, ruleCache).createAndApply()

    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    ingressUrls.addAll(collectionTagger.getIngressUrls())

    new AndroidXmlPermissionTagger(cpg, ruleCache, PermissionSourceRule.miniatureRuleList).createAndApply()

    logger.info("Done with tagging")

    cpg.tag
  }

}
