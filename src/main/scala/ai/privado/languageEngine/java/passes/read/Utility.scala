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

package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.TaggerCache
import ai.privado.dataflow.DuplicateFlowProcessor
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Member}

object Utility {

  def appendExtraNodesAndRetunNewFlow(
    taggerCache: TaggerCache,
    matchedTypeDeclFullName: String,
    node: AstNode
  ): List[(String, String, Path)] = {
    taggerCache
      .getTypeDeclMemberCacheItem(matchedTypeDeclFullName)
      .flatMap { case (sourceRuleId, members) =>
        members.map { sourceMatchingMemberL1 =>
          val memberNodes = taggerCache
            .getTypeDeclMemberCacheItem(sourceMatchingMemberL1.typeFullName)
            .getOrElse(sourceRuleId, Set.empty[Member]) + sourceMatchingMemberL1

          val newPath: Path = new Path(memberNodes.toList ::: List(node))
          val pathId        = DuplicateFlowProcessor.calculatePathId(newPath).get
          (pathId, sourceRuleId, newPath)
        }
      }
      .toList

//  taggerCache
//      .getTypeDeclMemberCacheItem(matchedTypeDeclFullName)
//      .map(item => (item._1, item._2.head))
//      .map(sourceRuleIdMemberTuple => {
//        val sourceRuleId = sourceRuleIdMemberTuple._1
//        val memberNodes = {
//          val sourceMatchingMemberL1 = sourceRuleIdMemberTuple._2
//          if (
//            taggerCache.typeDeclMemberCache.contains(sourceMatchingMemberL1.typeFullName) && taggerCache
//              .typeDeclMemberCache(sourceMatchingMemberL1.typeFullName)
//              .contains(sourceRuleId)
//          ) {
//            List(
//              taggerCache.getTypeDeclMemberCacheItem(sourceMatchingMemberL1.typeFullName).get(sourceRuleId).head,
//              sourceMatchingMemberL1
//            )
//          } else
//            List(sourceMatchingMemberL1)
//        }
//
//        val newPath: Path = new Path(memberNodes ::: List(node))
//        val pathId        = DuplicateFlowProcessor.calculatePathId(newPath).get
//        (pathId, sourceRuleId, newPath)
//      })
//      .toList
  }
}
