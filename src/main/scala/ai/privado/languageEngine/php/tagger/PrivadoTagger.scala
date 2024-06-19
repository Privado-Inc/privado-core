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

package ai.privado.languageEngine.php.tagger

import ai.privado.cache.{AppCache, DataFlowCache, DatabaseDetailsCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.php.tagger.collection.MethodFullNameCollectionTagger
import ai.privado.languageEngine.php.tagger.collection.AnnotationsCollectionTagger
import ai.privado.languageEngine.php.tagger.collection.ConfigCollectionTagger
import ai.privado.languageEngine.php.tagger.source.IdentifierTagger
import ai.privado.languageEngine.php.tagger.sink.APITagger
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.tagger.source.{DEDTagger, LiteralTagger}
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    rules: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder
  ): Traversal[Tag] = {
    logger.info("Beginning tagging")

    new DEDTagger(cpg, rules).createAndApply()
    new LiteralTagger(cpg, rules).createAndApply()
    new IdentifierTagger(cpg, rules, taggerCache).createAndApply()
    new RegularSinkTagger(cpg, rules, databaseDetailsCache).createAndApply()
    new AnnotationsCollectionTagger(cpg, rules).createAndApply()
    new ConfigCollectionTagger(cpg, rules, privadoInputConfig.sourceLocation.headOption.getOrElse("")).createAndApply()
    new MethodFullNameCollectionTagger(cpg, rules).createAndApply()
    new APITagger(cpg, rules, privadoInput = privadoInputConfig, appCache = appCache).createAndApply()

    logger.info("Finished tagging")
    cpg.tag
  }
}
