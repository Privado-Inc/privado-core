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

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{Constants, InternalTag}
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import ai.privado.languageEngine.java.tagger.source.Utility._

class InSensitiveCallTagger(cpg: Cpg, ruleCache: RuleCache, taggerCache: TaggerCache)
    extends ForkJoinParallelCpgPass[String](cpg) {

  override def generateParts(): Array[String] = taggerCache.typeDeclMemberCache.keys.toArray

  override def runOnPart(builder: DiffGraphBuilder, typeDeclFullName: String): Unit = {

    val (personalMembers, nonPersonalMembers) = getPersonalNonPersonalMembers(cpg, typeDeclFullName, taggerCache)
    val nonPersonalMembersRegexString         = nonPersonalMembers.mkString("(", "|", ")")
    val personalMembersRegexString            = s".*${personalMembers.mkString("(", "|", ")")}.*"
    if (nonPersonalMembers.nonEmpty) {
      getCallsMatchingReturnRegex(cpg, typeDeclFullName, nonPersonalMembersRegexString).dedup.foreach(
        storeForTag(builder, _, ruleCache)(
          InternalTag.INSENSITIVE_METHOD_RETURN.toString,
          "Data.Sensitive.NonPersonal.Method"
        )
      )

      getCallsMatchingNameRegex(cpg, typeDeclFullName, nonPersonalMembersRegexString).dedup.foreach(
        storeForTag(builder, _, ruleCache)(InternalTag.INSENSITIVE_SETTER.toString)
      )

      getFieldAccessCallsMatchingRegex(cpg, typeDeclFullName, nonPersonalMembersRegexString).foreach(impactedAccess => {
        if (impactedAccess.tag.nameExact(Constants.id).l.isEmpty) {
          storeForTag(builder, impactedAccess, ruleCache)(
            InternalTag.INSENSITIVE_FIELD_ACCESS.toString,
            "Data.Sensitive.NonPersonal.MemberAccess"
          )
        }
      })
    }

    if (personalMembers.nonEmpty)
      getCallsMatchingNameRegex(cpg, typeDeclFullName, personalMembersRegexString).dedup.foreach(
        storeForTag(builder, _, ruleCache)(InternalTag.SENSITIVE_SETTER.toString)
      )
  }
}
