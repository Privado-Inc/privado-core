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

import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, ViolationProcessingModel}
import ThreatUtility._
import ai.privado.exporter.ExporterUtility
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.xml.{Elem, XML}

object DataOnExternalStorage {

  private val USE_PERM_KEY        = "uses-permission"
  private val PERM_NAME_ATTRIBUTE = "name"

  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violations for external data storage
    * @param cpg
    *   cpg for code
    * @param androidManifestFile
    *   source filepath of manifest file
    * @return
    */
  def getViolations(cpg: Cpg, androidManifestFile: String): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    val occurrenceList  = ListBuffer[DataFlowSubCategoryPathExcerptModel]()
    val xml: Elem       = XML.loadFile(androidManifestFile)
    val permissionNodes = xml \\ USE_PERM_KEY

    if (hasDataElements(cpg) && permissionNodes.nonEmpty) {
      // we expect only one node, but a NodeSeq is returned
      permissionNodes.foreach {
        case permissionNode: Elem =>
          val nameAttribute = getAttribute(permissionNode.attributes, PERM_NAME_ATTRIBUTE) match {
            case Some(x) => x
            case _       => ""
          }
          if (
            nameAttribute == "android.permission.WRITE_EXTERNAL_STORAGE" || nameAttribute == "android.permission.MANAGE_EXTERNAL_STORAGE"
          ) {
            val occurrenceOutput =
              getOccurrenceObject(s"$PERM_NAME_ATTRIBUTE=\"$nameAttribute\"", nameAttribute, androidManifestFile)
            occurrenceList.append(occurrenceOutput)
          }
        case _ => // Node not found
      }

      val worldReadableCalls = cpg
        .argument(".*(MODE_WORLD_READABLE|MODE_WORLD_WRITABLE).*")
        .inCall
        .whereNot(_.methodFullName(".*<operator>.*"))
      if (worldReadableCalls.nonEmpty) {
        val occurrences = ExporterUtility.convertPathElements(worldReadableCalls.toList)
        occurrenceList.addAll(occurrences)
      }
    }

    // threat exists if occurrences are non-empty
    (occurrenceList.nonEmpty, transformOccurrenceList(occurrenceList.toList))
  }
}
