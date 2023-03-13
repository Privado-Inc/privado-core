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

package ai.privado.languageEngine.java.tagger.source

import ai.privado.cache.TaggerCache
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.language._

object Utility {

  def getCallsMatchingReturnRegex(cpg: Cpg, typeDeclFullName: String, regexString: String): List[Call] = {
    implicit val resolver: ICallResolver = NoResolve

    cpg.typeDecl
      .where(_.fullName(typeDeclFullName))
      .method
      .block
      .astChildren
      .isReturn
      .code(s"return (?i)(this.)?$regexString(;)?")
      .method
      .callIn
      .l ++ cpg.identifier
      .typeFullName(typeDeclFullName)
      .astParent
      .isCall
      .name(s"(?i)(get|is)$regexString")
      .l
  }

  def getFieldAccessCallsMatchingRegex(cpg: Cpg, typeDeclFullName: String, regexString: String): List[Call] = {
    implicit val resolver: ICallResolver = NoResolve
    cpg.method
      .fullNameExact(Operators.fieldAccess, Operators.indirectFieldAccess)
      .callIn
      .where(_.argument(1).isIdentifier.typeFullName(typeDeclFullName))
      .where(_.argument(2).code(s"(?i)$regexString"))
      .l
  }

  def getPersonalNonPersonalMembers(cpg: Cpg, typeDeclFullName: String): (Set[String], Set[String]) = {
    val typeDeclNode       = cpg.typeDecl.where(_.fullName(typeDeclFullName)).l
    val allMembers         = typeDeclNode.member.name.toSet
    val personalMembers    = TaggerCache.typeDeclMemberCache(typeDeclFullName).values.name.toSet
    val nonPersonalMembers = allMembers.diff(personalMembers)
    (personalMembers, nonPersonalMembers)
  }

}
