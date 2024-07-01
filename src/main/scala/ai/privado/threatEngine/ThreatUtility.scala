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

package ai.privado.threatEngine

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.exporter.ExporterUtility
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities
import io.shiftleft.semanticcpg.language.*
import better.files.File
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode, Tag}
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks.{break, breakable}
import scala.xml.MetaData

object ThreatUtility {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Convert Individual path element
    * @param occurrenceList
    *   The occurrence list
    * @return
    *   list of ViolationProcessingModel
    */
  def transformOccurrenceList(
    occurrenceList: List[DataFlowSubCategoryPathExcerptModel]
  ): List[ViolationProcessingModel] = {
    occurrenceList.map(occurrence => ViolationProcessingModel("", Some(occurrence), None))
  }

  def hasDataElements(cpg: Cpg): Boolean = {
    val taggedSources = cpg.tag
      .nameExact(Constants.catLevelOne)
      .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
      .l
    taggedSources.nonEmpty
  }

  /** Returns matching line number from the file
    * @param fileName
    *   name of file
    * @param matchingText
    *   match text
    * @return
    */
  def getLineNumberOfMatchingEditText(fileName: String, matchingText: String): Int = {
    var matchedLineNumber = -2
    try {
      val lines = File(fileName).lines.toList
      breakable {
        for (lineNumber <- 1 until lines.size) {
          if (lines(lineNumber).contains(matchingText)) {
            matchedLineNumber = lineNumber
            break()
          }
        }
      }
    } catch {
      case e: Exception => logger.debug("Exception", e)
    }
    matchedLineNumber + 1
  }

  /** Gets the value of a key from attributes
    * @param attributes
    *   the attributes of xml node
    * @return
    *   Option[string] value of attribute
    */
  def getAttribute(attributes: MetaData, key: String): Option[String] = {
    val filteredAttrs = attributes.filter(_.key == key)
    if (filteredAttrs.nonEmpty) Some(filteredAttrs.head.value.head.text) else None
  }

  def getOccurrenceObject(
    matchingTextForLine: String,
    sample: String,
    filename: String,
    excerptPostfix: String = ""
  ): DataFlowSubCategoryPathExcerptModel = {

    val lineNumber        = getLineNumberOfMatchingEditText(filename, matchingTextForLine)
    val _lineNumberToDump = if (filename.endsWith(".cs")) lineNumber + 1 else lineNumber
    val excerpt           = Utilities.dump(filename, Some(_lineNumberToDump), "") + "\n" + excerptPostfix + "\n"
    DataFlowSubCategoryPathExcerptModel(sample, _lineNumberToDump, -1, filename, excerpt)
  }

  def getOccurrenceObjectWithCustomExcerpt(
    excerpt: String,
    sample: String,
    filename: String,
    excerptPostfix: String = ""
  ): DataFlowSubCategoryPathExcerptModel = {
    val lineNumber        = getLineNumberOfMatchingEditText(filename, sample)
    val _lineNumberToDump = if (filename.endsWith(".cs")) lineNumber + 1 else lineNumber
    DataFlowSubCategoryPathExcerptModel(sample, _lineNumberToDump, -1, filename, excerpt + "\n" + excerptPostfix + "\n")
  }

  def getSourceNode(cpg: Cpg, sourceId: String): Option[(String, CfgNode)] = {
    def filterBySource(tag: Traversal[Tag]): Traversal[Tag] =
      tag.where(_.nameExact(Constants.id)).where(_.valueExact(sourceId))

    Try(cpg.tag.where(filterBySource).identifier.head) match {
      case Success(identifierNode) => Some(sourceId, identifierNode)
      case Failure(_) =>
        Try(cpg.tag.where(filterBySource).literal.head) match {
          case Success(literalNode) => Some(sourceId, literalNode)
          case Failure(_) =>
            Try(cpg.tag.where(filterBySource).call.head) match {
              case Success(callNode) => Some(sourceId, callNode)
              case Failure(_)        => None
            }
        }
    }
  }

  def getPIINameFromSourceId(input: String): String = {
    val words = input.split("\\.")
    words.lastOption.getOrElse(input)
  }

  def convertToViolationProcessingModelAndAddToViolatingFlows(
    sample: Option[String],
    node: AstNode,
    violatingFlows: ListBuffer[ViolationProcessingModel],
    sourceId: String,
    details: Option[String],
    appCache: AppCache,
    ruleCache: RuleCache
  ) = {
    val occurrence = ExporterUtility.convertIndividualPathElement(node, appCache = appCache, ruleCache = ruleCache)
    if (occurrence.isDefined) {
      val newOccurrence = DataFlowSubCategoryPathExcerptModel(
        if sample.isDefined then sample.get else occurrence.get.sample,
        occurrence.get.lineNumber,
        occurrence.get.columnNumber,
        occurrence.get.fileName,
        occurrence.get.excerpt
      )
      violatingFlows.append(ViolationProcessingModel(sourceId, Some(newOccurrence), details))
    }
  }
}
