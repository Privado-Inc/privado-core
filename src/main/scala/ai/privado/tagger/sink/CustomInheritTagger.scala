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
import ai.privado.model.RuleInfo
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

abstract class CustomInheritTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val typeDeclNodeL1 = CustomTaggerUtility.getImpactedTypeDeclNodeByExtends(cpg, ruleInfo.patterns.head)
    // We are calling the function getImpactedTypeDeclNode again to get classes following below format
    // public interface BillableUsageRepository extends EnrichmentRepository<BillableUsageEntity>
    // EnrichmentRepository extends JPARepository
    val typeDeclNode = typeDeclNodeL1 ++ CustomTaggerUtility.getImpactedTypeDeclNodeByExtends(
      cpg,
      typeDeclNodeL1.fullName.mkString("(", "|", ")")
    )
    if (typeDeclNode.nonEmpty) {
      typeDeclNode.fullName.dedup.foreach(typeDeclName => {
        val callNodes = cpg.call.methodFullName(typeDeclName + ".*" + ruleInfo.patterns(1)).l
        callNodes.foreach(callNode => addRuleTags(builder, callNode, ruleInfo, ruleCache))
      })
    }
  }
}
