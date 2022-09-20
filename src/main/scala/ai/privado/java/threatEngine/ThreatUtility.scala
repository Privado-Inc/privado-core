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

package ai.privado.java.threatEngine

import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities
import io.shiftleft.semanticcpg.language._
import better.files.File
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

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
    occurrenceList.map(occurrence => ViolationProcessingModel("", occurrence))
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

    val lineNumber = getLineNumberOfMatchingEditText(filename, matchingTextForLine)
    val excerpt    = Utilities.dump(filename, Some(lineNumber)) + "\n" + excerptPostfix + "\n"
    DataFlowSubCategoryPathExcerptModel(sample, lineNumber, -1, filename, excerpt)
  }

  def getOccurrenceObjectWithCustomExcerpt(
    excerpt: String,
    sample: String,
    filename: String,
    excerptPostfix: String = ""
  ): DataFlowSubCategoryPathExcerptModel = {
    val lineNumber = getLineNumberOfMatchingEditText(filename, sample)
    DataFlowSubCategoryPathExcerptModel(sample, lineNumber, -1, filename, excerpt + "\n" + excerptPostfix + "\n")
  }

}
