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

package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache, TaggerCache}
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, RuleInfo, ViolationPolicyDetailsModel}
import ai.privado.semantic.Language.finder
import ai.privado.utility.Utilities.dump
import io.shiftleft.codepropertygraph.generated.nodes._
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language._
import better.files.File

import scala.collection.mutable

object ExporterUtility {

  /** Convert List of path element schema object
    */
  def convertPathElements(nodes: List[AstNode], sourceId: String = ""): List[DataFlowSubCategoryPathExcerptModel] = {
    val sizeOfList = nodes.size
    nodes.zipWithIndex.flatMap { case (node, index) =>
      if (
        index == 0 && node.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.DERIVED_SOURCES.name).nonEmpty
      ) {
        val typeFullName = Traversal(node).isIdentifier.typeFullName.headOption.getOrElse("")
        // Going 1 level deep for derived sources to add extra nodes
        TaggerCache.typeDeclMemberNameCache(typeFullName).get(sourceId) match {
          case Some(m) =>
            val typeFullNameLevel2 = m.typeFullName
            TaggerCache.typeDeclMemberNameCache.getOrElse(typeFullNameLevel2, mutable.HashMap()).get(sourceId) match {
              case Some(m2) =>
                // Going 2 level deep for derived sources to add extra nodes
                convertIndividualPathElement(m2) ++ convertIndividualPathElement(m) ++ convertIndividualPathElement(
                  node,
                  index,
                  sizeOfList
                )
              case _ =>
                convertIndividualPathElement(m) ++ convertIndividualPathElement(node, index, sizeOfList)
            }

          case _ =>
            convertIndividualPathElement(node, index, sizeOfList)
        }
      } else
        convertIndividualPathElement(node, index, sizeOfList)
    }
  }

  /** Convert Individual path element
    * @param node
    *   \- cfg node
    * @param index
    *   \- index of the list item if any
    * @param sizeOfList
    *   \- size of the list if any
    * @return
    */
  def convertIndividualPathElement(
    node: AstNode,
    index: Int = -1,
    sizeOfList: Int = -1
  ): Option[DataFlowSubCategoryPathExcerptModel] = {
    val sample = node.code
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
      case a @ (_: Identifier | _: Literal | _: MethodParameterIn | _: Call | _: FieldIdentifier | _: Member) =>
        a.file.name.head
      case a => a.location.filename
    }
    val absoluteFileName = {
      val file = File(fileName)
      if (file.exists)
        fileName
      else {
        if (AppCache.scanPath.endsWith("/"))
          AppCache.scanPath + fileName
        else
          AppCache.scanPath + "/" + fileName
      }
    }

    if (fileName == "<empty>" || sample == "<empty>")
      None
    else {
      val methodFullName = Traversal(node).isCall.methodFullName.headOption.getOrElse("")
      val excerpt        = dump(absoluteFileName, node.lineNumber, methodFullName)
      Some(DataFlowSubCategoryPathExcerptModel(sample, lineNumber, columnNumber, fileName, excerpt))
    }
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
