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

import ai.privado.languageEngine.javascript.tagger.sink.RegularSinkTagger
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import ai.privado.model.{ConfigAndRules, NodeType}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.sink.APITagger
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

    println(s"${Calendar.getInstance().getTime} - LiteralTagger invoked...")
    new LiteralTagger(cpg).createAndApply()
    println(s"${Calendar.getInstance().getTime} - IdentifierTagger invoked...")
    new IdentifierTagger(cpg).createAndApply()
    println(s"${Calendar.getInstance().getTime} - RegularSinkTagger invoked...")
    new RegularSinkTagger(cpg).createAndApply()
    println(s"${Calendar.getInstance().getTime} - APITagger invoked...")
    new APITagger(cpg).createAndApply()

    logger.info("Done with tagging")

    cpg.tag
  }

}
