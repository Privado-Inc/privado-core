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

package ai.privado.threatEngine

import ai.privado.cache.RuleCache
import ai.privado.model.RuleInfo
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import ai.privado.threatEngine.ThreatUtility._
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.xml.{Elem, MetaData, XML}

object SensitiveInputMask {

  private val INPUT_KEY                = "EditText"
  private val INPUT_ID_ATTRIBUTE       = "id"
  private val INPUT_TYPE_ATTRIBUTE     = "inputType"
  private val INPUT_PASSWORD_ATTRIBUTE = "password"

  private val logger = LoggerFactory.getLogger(getClass)

  private val sensitiveInputTypes = List("numberPassword", "textPassword", "textWebPassword")

  private val sensitiveDataElementsForMasking: Set[String] = Set(
    "Data.Sensitive.AccountData.AccountID",
    "Data.Sensitive.AccountData.AccountPassword",
    "Data.Sensitive.BackgroundCheckDetails.CompensationHistory",
    "Data.Sensitive.BackgroundCheckDetails.CreditHistory",
    "Data.Sensitive.FinancialData.BankAccountDetails",
    "Data.Sensitive.FinancialData.CardNumber",
    "Data.Sensitive.FinancialData.VPAAddress",
    "Data.Sensitive.FinancialData.RetirementAccountInformation",
    "Data.Sensitive.FinancialData.PayrollInformation",
    "Data.Sensitive.FinancialData.Salary",
    "Data.Sensitive.HealthData.HealthInsuranceNumber",
    "Data.Sensitive.HealthData.DisabilityorSpecificCondition",
    "Data.Sensitive.HealthData.IllnessorMedicalCondition",
    "Data.Sensitive.NationalIdentificationNumbers.Passport",
    "Data.Sensitive.NationalIdentificationNumbers.DrivingLicense",
    "Data.Sensitive.NationalIdentificationNumbers.SocialSecurityNumber",
    "Data.Sensitive.NationalIdentificationNumbers.PANNumber",
    "Data.Sensitive.NationalIdentificationNumbers.AadharNumber",
    "Data.Sensitive.PersonalIdentification.EmployeeCode",
    "Data.Sensitive.VehicleData.VehicleRegistrationNumber",
    "Data.Sensitive.VehicleData.LicensePlate"
  )

  /** Fetch all the violations for unmasked sensitive input
    * @param cpg
    *   Cpg for repo
    * @param repoPath
    *   Path to repo
    * @return
    */
  def getViolations(cpg: Cpg, repoPath: String): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    val occurrenceList = ListBuffer[DataFlowSubCategoryPathExcerptModel]()
    getAllFilesRecursively(repoPath, Set(".xml")) match {
      case Some(sourceFileNames) =>
        sourceFileNames.foreach(sourceFile => {
          val xml: Elem     = XML.loadFile(sourceFile)
          val editTextNodes = xml \\ INPUT_KEY
          if (editTextNodes.nonEmpty) {
            editTextNodes.foreach {
              case editText: Elem =>
                // check if EditText represents a sensitive rule
                if (isInputSensitiveById(editText.attributes, RuleCache.getRule.sources)) {
                  // check if input is not masked
                  if (!isInputMasked(editText.attributes)) {
                    // get values - these are confirmed to exist at this point
                    val idAttribute        = editText.attributes.filter(_.key == INPUT_ID_ATTRIBUTE).value.head
                    val inputTypeAttribute = editText.attributes.filter(_.key == INPUT_TYPE_ATTRIBUTE)
                    // text is default value in absence of attribute - also unmasked
                    val inputTypeValue = if (inputTypeAttribute.nonEmpty) inputTypeAttribute.value.head.text else "text"

                    val occurrenceOutput =
                      getOccurrenceObject(
                        s"${INPUT_ID_ATTRIBUTE}=\"${idAttribute.text}\"",
                        s"${INPUT_TYPE_ATTRIBUTE}=\"${inputTypeValue}\"",
                        sourceFile
                      )
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
    *   xml node attributes
    * @param sources
    *   source rules
    * @return
    */
  private def isInputSensitiveById(attributes: MetaData, sources: List[RuleInfo]): Boolean = {
    getAttribute(attributes, INPUT_ID_ATTRIBUTE) match {
      case Some(id) =>
        id.stripPrefix("@+id/")
        val matchedSources =
          sources.filter(source => sensitiveDataElementsForMasking(source.id) && id.matches(source.patterns.head))
        matchedSources.nonEmpty
      case _ => false
    }
  }

  private def isInputMasked(attributes: MetaData): Boolean = {
    getAttribute(attributes, INPUT_TYPE_ATTRIBUTE) match {
      case Some(inputTypeAttribute) =>
        var isMasked = false
        sensitiveInputTypes.foreach { inputType =>
          if (inputTypeAttribute.contains(inputType)) isMasked = true
        }
        isMasked
      case _ =>
        // older schemas defined the "password" tag
        getAttribute(attributes, INPUT_PASSWORD_ATTRIBUTE) match {
          case Some(password) if password == "true" => true
          case _                                    => false
        }
    }
  }
}
