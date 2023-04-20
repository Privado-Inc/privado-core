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

package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.RuleCache
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import ai.privado.model.RuleInfo
import ai.privado.utility.Utilities._
import ThreatUtility._
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, MetaData, XML}

import scala.collection.mutable.ListBuffer

object KeyboardCache {

  private val ID         = "android:id"
  private val INPUT_TYPE = "android:inputType"
  private val logger     = LoggerFactory.getLogger(getClass)

  val sensitiveInputTypeList: List[String] = List(
    "numberPassword",
    "phone",
    "textEmailAddress",
    "textPassword",
    "textPersonName",
    "textPostalAddress",
    "textWebEmailAddress",
    "textWebPassword"
  )

  /** Fetch all the violations which violate Key-board cache threat
    * @param repoPath
    *   \- path of repo
    * @return
    */
  def getViolations(ruleCache: RuleCache, repoPath: String): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    val occurrenceList = ListBuffer[DataFlowSubCategoryPathExcerptModel]()
    getAllFilesRecursively(repoPath, Set(".xml"), ruleCache) match {
      case Some(sourceFileNames) =>
        sourceFileNames.foreach(sourceFile => {
          val xml: Elem = XML.loadFile(sourceFile)
          val editText  = xml \ "EditText"
          if (editText.nonEmpty) {
            editText.foreach {
              case Elem(prefix, label, attributes, scope, child @ _*) =>
                if (isSensitiveInputTypePresent(attributes) || isSensitiveId(attributes, ruleCache.getRule.sources)) {
                  if (!isTextNoSuggestionsInInputTypePresent(attributes)) {
                    val idAttribute = attributes.filter(attribute => attribute.prefixedKey == ID).value.head
                    val occurrenceOutput =
                      getOccurrenceObject(ID + "=\"" + idAttribute.text + "\"", idAttribute.text, sourceFile)
                    occurrenceList.append(occurrenceOutput)
                  }
                }
              case _ => // Node not found
            }
          }
        })
      case None => // repo is not correct
    }

    val sanitizedOccurrenceList = transformOccurrenceList(occurrenceList.toList)

    // threat exists if occurrences are non-empty
    (sanitizedOccurrenceList.nonEmpty, sanitizedOccurrenceList)
  }

  /** Checks if the field id is sensitive
    * @param attributes
    * @param sources
    * @return
    */
  private def isSensitiveId(attributes: MetaData, sources: List[RuleInfo]): Boolean = {
    try {
      val elementId = attributes.filter(attribute => attribute.prefixedKey == ID).head.value.head.text
      sources
        .map(source => {
          var androidId = elementId
          if (elementId.startsWith("@+id/"))
            androidId = elementId.slice(5, elementId.size)
          androidId.matches(source.combinedRulePattern)
        })
        .foldLeft(false)((a, b) => a || b)
    } catch {
      case _: Exception => false
    }

  }

  /** Checks if the field "inputType" contains item from sensitiveInputTypeList
    *
    * @param attributes
    * @return
    */
  private def isSensitiveInputTypePresent(attributes: MetaData, attributeType: String = INPUT_TYPE): Boolean = {
    Try(
      attributes.filter(attribute =>
        attribute.prefixedKey == attributeType && isAttributeValuePresentInSensitiveInputTypeList(
          attribute.value.head.text
        )
      )
    ) match {
      case Success(filteredAttributes) => filteredAttributes.nonEmpty

      case Failure(exception) =>
        logger.debug("Exception : ", exception)
        false
    }
  }

  /** Helper function to check if the attribute value contains any sensitive Type
    * @param attributeValue
    * @return
    */
  private def isAttributeValuePresentInSensitiveInputTypeList(attributeValue: String) = {
    Try(
      attributeValue.split("\\|").map(value => sensitiveInputTypeList.contains(value)).reduce((a, b) => a || b)
    ) match {
      case Success(result) => result
      case Failure(e) =>
        logger.debug("Exception : ", e)
        false
    }
  }

  private def isTextNoSuggestionsInInputTypePresent(attributes: MetaData): Boolean = {
    Try(
      attributes
        .filter(attribute => attribute.prefixedKey == INPUT_TYPE)
        .head
        .value
        .text
        .split("\\|")
        .contains("textNoSuggestions")
    ) match {
      case Success(value) => value
      case Failure(e) =>
        logger.debug("Exception : ", e)
        false
    }

  }
}
