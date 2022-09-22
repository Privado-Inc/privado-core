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

package ai.privado.java.exporter

import ai.privado.cache.RuleCache
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, RuleInfo, ViolationPolicyDetailsModel}
import ai.privado.utility.Utilities.dump
import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  CfgNode,
  FieldIdentifier,
  Identifier,
  Literal,
  MethodParameterIn
}
import io.shiftleft.semanticcpg.language.toExtendedNode
import ai.privado.semantic.Language.finder
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._

object ExporterUtility {

  /** Convert List of path element schema object
    */
  def convertPathElements(nodes: List[CfgNode]): List[DataFlowSubCategoryPathExcerptModel] = {
    nodes.flatMap(node => convertIndividualPathElement(node))
  }

  /** Convert Individual path element
    * @param node
    *   \- cfg node
    * @return
    */
  def convertIndividualPathElement(node: CfgNode): Option[DataFlowSubCategoryPathExcerptModel] = {
    val nodeLocation = node.location
    val sample       = node.code
    val lineNumber: Int = {
      node.lineNumber match {
        case Some(n) => n
        case None    => -1
      }
    }
    val columnNumber: Int = {
      node.columnNumber match {
        case Some(n) => n
        case None    => -1
      }
    }
    val fileName = Traversal(node).head match {
      case a @ (_: Identifier | _: Literal | _: MethodParameterIn | _: Call | _: FieldIdentifier) => a.file.name.head
      case a                                                                                      => a.location.filename
    }
    val excerpt = dump(fileName, node.lineNumber)

    /*
    if (nodeLocation.filename == "<empty>" || nodeLocation.symbol == "<empty>")
      None
    else
     */
    // TODO remove this comment once fileName starts getting reflected in fieldIdentifierNode
    Some(DataFlowSubCategoryPathExcerptModel(sample, lineNumber, columnNumber, fileName, excerpt))
  }

  def getRuleInfoForExporting(ruleId: String): RuleInfo = {
    RuleCache.getRuleInfo(ruleId) match {
      case Some(rule) =>
        RuleInfo(rule.id, rule.name, rule.category, rule.domains, rule.sensitivity, rule.isSensitive, rule.tags)
      case None => RuleInfo("", "", "", Array[String](), "", isSensitive = false, Map[String, String]())
    }
  }

  def getPolicyInfoForExporting(policyOrThreatId: String): Option[ViolationPolicyDetailsModel] = {
    RuleCache.getPolicyOrThreat(policyOrThreatId) match {
      case Some(policyOrThreat) =>
        Some(
          ViolationPolicyDetailsModel(
            policyOrThreat.name,
            policyOrThreat.policyOrThreatType.toString,
            policyOrThreat.description,
            policyOrThreat.fix,
            { if (policyOrThreat.action != null) policyOrThreat.action.toString else "" },
            policyOrThreat.tags
          )
        )
      case None => None
    }
  }

}
