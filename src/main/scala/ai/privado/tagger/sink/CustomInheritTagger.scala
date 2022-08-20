/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.tagger.sink

import ai.privado.tagger.PrivadoSimplePass
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities._

class CustomInheritTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    val typeDeclNode = cpg.typeDecl
      .filter(
        _.inheritsFromTypeFullName
          .map(inheritsFrom => inheritsFrom.matches(ruleInfo.patterns.head))
          .foldLeft(false)((a, b) => a || b)
      )
      .l
    if (typeDeclNode.nonEmpty) {
      typeDeclNode.fullName.dedup.foreach(typeDeclName => {
        val callNodes = cpg.call.methodFullName(typeDeclName + ".*" + ruleInfo.patterns(1)).l
        callNodes.foreach(callNode => addRuleTags(builder, callNode, ruleInfo))
      })
    }
  }
}
