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
import ThreatUtility._
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.xml.{Elem, XML}

object SensitiveDataBackup {

  private val KEY              = "application"
  private val ALLOW_BACKUP_KEY = "android:allowBackup"
  private val logger           = LoggerFactory.getLogger(getClass)

  /** Check for violations for sensitive data backup threat
    * @param androidManifestFile
    *   source filepath of manifest file
    * @return
    */
  def getViolations(cpg: Cpg, androidManifestFile: String): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    val occurrenceList   = ListBuffer[DataFlowSubCategoryPathExcerptModel]()
    val xml: Elem        = XML.loadFile(androidManifestFile)
    val applicationNodes = xml \\ KEY

    if (hasDataElements(cpg) && applicationNodes.nonEmpty) {
      // we expect only one node, but a NodeSeq is returned
      applicationNodes.foreach {
        case Elem(prefix, label, attributes, scope, child @ _*) =>
          val backupAttribute = getAttribute(attributes, ALLOW_BACKUP_KEY) match {
            case Some(x) => x
            case _       => ""
          }
          if (backupAttribute == "true") {
            val occurrenceOutput = getOccurrenceObject(
              ALLOW_BACKUP_KEY + "=\"" + backupAttribute + "\"",
              s"${ALLOW_BACKUP_KEY}=\"${backupAttribute}\"",
              androidManifestFile
            )
            occurrenceList.append(occurrenceOutput)
          }
        case _ => // Node not found
      }
    }

    val sanitizedOccurrenceList = transformOccurrenceList(occurrenceList.toList)
    // threat exists if occurrences are non-empty
    (sanitizedOccurrenceList.nonEmpty, sanitizedOccurrenceList)
  }
}
