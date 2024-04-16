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

import ai.privado.cache.{AppCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.php.tagger.source.IdentifierTagger
import ai.privado.languageEngine.php.tagger.sink.{APITagger, CollectionTagger}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.tagger.source.LiteralTagger
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
    appCache: AppCache
  ): Traversal[Tag] = {
    logger.info("Beginning tagging")

    new LiteralTagger(cpg, rules).createAndApply()
    new IdentifierTagger(cpg, rules, taggerCache).createAndApply()
    new RegularSinkTagger(cpg, rules).createAndApply()
    new CollectionTagger(cpg, rules).createAndApply()
    new APITagger(cpg, rules, privadoInput = privadoInputConfig, appCache = appCache).createAndApply()

    logger.info("Finished tagging")
    cpg.tag
  }
}
