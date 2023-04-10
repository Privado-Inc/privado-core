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

package ai.privado.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._

class LogShareSinkTagger(cpg: Cpg) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  val logSharingThirdPartySinkId = "ThirdParties.SDK.Sentry"
  val higherOrderLeakgeSinkId    = "Leakages.Log.(Error|Exception)"

  val callNodeTaggedWithErrorOrExceptionLog: List[Call] = {

    if (cpg.call.tag.nameExact(Constants.id).valueExact(logSharingThirdPartySinkId).nonEmpty) {
      cpg.call
        .where(
          _.tag
            .nameExact(Constants.id)
            .value(higherOrderLeakgeSinkId)
        )
        .l
    } else
      List.empty
  }

  override def generateParts(): Array[RuleInfo] = {
    RuleCache.getRule.sinks
      .filter(rule => rule.id.equals(logSharingThirdPartySinkId))
      .toArray
  }
  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    callNodeTaggedWithErrorOrExceptionLog.foreach(sink => addRuleTags(builder, sink, ruleInfo))
  }
}
