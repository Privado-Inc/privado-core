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
import ai.privado.model.{Constants, InternalTag}
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.{ForkJoinParallelCpgPass}
import io.shiftleft.semanticcpg.language._

class IdentifierNonMemberTagger(cpg: Cpg) extends ForkJoinParallelCpgPass[String](cpg) {

  override def generateParts(): Array[String] = TaggerCache.typeDeclMemberCache.keys.toArray

  override def runOnPart(builder: DiffGraphBuilder, typeDeclValue: String): Unit = {

    implicit val resolver: ICallResolver = NoResolve
    val typeDeclNode                     = cpg.typeDecl.where(_.fullName(typeDeclValue)).l
    val allMembers                       = typeDeclNode.member.name.toSet
    val personalMembers                  = TaggerCache.typeDeclMemberCache(typeDeclValue).values.name.toSet
    val nonPersonalMembers               = allMembers.diff(personalMembers)

    val nonPersonalMembersRegex = nonPersonalMembers.mkString("|")
    if (nonPersonalMembersRegex.nonEmpty) {
      // Below same regex is repeated in Utilities-->generateNonPersonalMemberSemantics
      val impactedMethods = typeDeclNode.method.block.astChildren.isReturn
        .code(s"return (?i)(this.)?($nonPersonalMembersRegex)(;)?")
        .method
        .callIn
        .l ++ cpg.identifier
        .typeFullName(typeDeclValue)
        .astParent
        .isCall
        .name(s"(?i)(get|is)($nonPersonalMembersRegex)")
        .l

      impactedMethods.dedup.foreach(impactedReturnCall => {
        storeForTag(builder, impactedReturnCall)(
          InternalTag.NON_SENSITIVE_METHOD_RETURN.toString,
          "Data.Sensitive.NonPersonal.Method"
        )
      })
    }

    val impactedFieldAccess = cpg.method
      .fullNameExact(Operators.fieldAccess, Operators.indirectFieldAccess)
      .callIn
      .where(_.argument(1).isIdentifier.typeFullName(typeDeclValue))
      .where(_.argument(2).code("(?i)(" + nonPersonalMembersRegex + ")"))
      .l

    impactedFieldAccess.foreach(impactedAccess => {
      if (impactedAccess.tag.nameExact(Constants.id).l.isEmpty) {
        storeForTag(builder, impactedAccess)(
          InternalTag.NON_SENSITIVE_FIELD_ACCESS.toString,
          "Data.Sensitive.NonPersonal.MemberAccess"
        )
      }
    })
  }
}
