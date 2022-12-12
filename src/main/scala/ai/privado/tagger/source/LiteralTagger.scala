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
 */

package ai.privado.tagger.source

import ai.privado.model.InternalTag
import ai.privado.tagger.PrivadoSimplePass
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

class LiteralTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    // Step 1.2
    val literals = cpg.literal.code("\"(" + ruleInfo.combinedRulePattern + ")\"").l
    literals.foreach(literal => {
      storeForTag(builder, literal)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
      addRuleTags(builder, literal, ruleInfo)
    })
  }
}
