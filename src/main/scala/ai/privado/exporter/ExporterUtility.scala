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
import ai.privado.utility.Utilities
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
      val currentNodeModel = convertIndividualPathElement(node, index, sizeOfList)
      if (
        index == 0 && node.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.DERIVED_SOURCES.name).nonEmpty
      ) {
        val typeFullName = Traversal(node).isIdentifier.typeFullName.headOption.getOrElse("")
        // Going 1 level deep for derived sources to add extra nodes
        TaggerCache.typeDeclMemberCache.getOrElse(typeFullName, mutable.HashMap[String, Member]()).get(sourceId) match {
          case Some(member) =>
            val typeFullNameLevel2 = member.typeFullName
            TaggerCache.typeDeclMemberCache
              .getOrElse(typeFullNameLevel2, mutable.HashMap[String, Member]())
              .get(sourceId) match {
              case Some(member2) =>
                // Going 2 level deep for derived sources to add extra nodes
                convertIndividualPathElement(
                  member2,
                  messageInExcerpt = generateDSMemberMsg(member2.name, typeFullNameLevel2)
                ) ++ convertIndividualPathElement(
                  member,
                  messageInExcerpt = generateDSMemberMsg(member.name, typeFullName)
                ) ++ currentNodeModel
              case _ =>
                convertIndividualPathElement(
                  member,
                  messageInExcerpt = generateDSMemberMsg(member.name, typeFullName)
                ) ++ currentNodeModel
            }

          case _ => // Checking if 2nd level is of Extends type
            TaggerCache.typeDeclExtendingTypeDeclCache
              .getOrElse(typeFullName, mutable.HashMap[String, TypeDecl]())
              .get(sourceId) match {
              case Some(typeDecl) => // Fetching information for the 2nd level member node
                TaggerCache.typeDeclMemberCache
                  .getOrElse(typeDecl.fullName, mutable.HashMap[String, Member]())
                  .get(sourceId) match {
                  case Some(member) =>
                    val currentTypeDeclNode = // Fetching the current TypeDecl node
                      TaggerCache.typeDeclDerivedByExtendsCache.get(typeFullName)
                    convertIndividualPathElement(
                      member,
                      messageInExcerpt = generateDSMemberMsg(member.name, typeDecl.fullName)
                    ) ++ convertIndividualPathElement(
                      currentTypeDeclNode.get,
                      messageInExcerpt = generateDSExtendsMsg(typeDecl.name, typeFullName)
                    ) ++ currentNodeModel
                  case _ =>
                    currentNodeModel
                }
              case _ =>
                currentNodeModel
            }
        }
      } else
        currentNodeModel
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
    sizeOfList: Int = -1,
    messageInExcerpt: String = ""
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
    val fileName = Utilities.getFileNameForNode(node)
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

    if (fileName.equals(Constants.EMPTY) || sample.equals(Constants.EMPTY))
      None
    else {
      val message = {
        if (Traversal(node).isCall.nonEmpty) {
          val methodFullName  = Traversal(node).isCall.methodFullName.headOption.getOrElse("")
          val methodInterface = methodFullName.split(":").headOption.getOrElse("")
          if (methodInterface.contains("unresolved") || methodInterface.contains("<operator>")) ""
          else methodInterface
        } else if (Traversal(node).isIdentifier.nonEmpty)
          Traversal(node).isIdentifier.typeFullName.headOption.getOrElse("")
        else
          messageInExcerpt
      }
      val excerpt = dump(absoluteFileName, node.lineNumber, message)
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

  /** Helper function to generate message
    * @param memberName
    * @param typeDeclFullName
    * @return
    */
  private def generateDSMemberMsg(memberName: String, typeDeclFullName: String): String = {
    s"'$memberName' is a member of '$typeDeclFullName' class"
  }

  /** Helper function to generate message
    * @param typeDeclName
    * @param typeDeclFullName
    * @return
    */
  private def generateDSExtendsMsg(typeDeclName: String, typeDeclFullName: String): String = {
    s"'$typeDeclName' class is inherited by '$typeDeclFullName' class"
  }

}
