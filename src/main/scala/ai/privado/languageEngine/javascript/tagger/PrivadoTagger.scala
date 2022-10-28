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

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.javascript.tagger.collection.CollectionTagger
import ai.privado.languageEngine.javascript.tagger.sink.RegularSinkTagger
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import ai.privado.model.ConfigAndRules
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.APITagger
import ai.privado.tagger.source.LiteralTagger
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(rules: ConfigAndRules): Traversal[Tag] = {

    logger.info("Starting tagging")

    new LiteralTagger(cpg).createAndApply()
    new IdentifierTagger(cpg).createAndApply()

    new RegularSinkTagger(cpg).createAndApply()
    new APITagger(cpg).createAndApply()
    new CollectionTagger(cpg, RuleCache.getRule.sources).createAndApply()

    logger.info("Done with tagging")

    cpg.tag
  }

}
