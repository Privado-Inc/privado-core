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

package ai.privado.utility

import better.files.File
import scala.util.{Success, Try}
import ai.privado.model.Constants
import scala.collection.mutable.ListBuffer
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.utility.Utilities.getFileNameForNode

object UnresolvedReportUtility {
    def reportUnresolvedMethods(xtocpg: Try[Cpg], filename: String): Unit = {
    var total = 0
    var unresolvedSignatures = 0
    var unresolvedNamespaces = 0
    var unresolvedSignaturesList = ListBuffer[String]()
    var unresolvedNamespacesList = ListBuffer[String]()

    val java_unresolved_signature = "(?i)(.*)(unresolved)(signature)(.*)"
    val java_unresolved_namespace = "(?i)(.*)(unresolved)(namespace)(.*)"
    val python_unresolved = "(?i)(.*)(unknownfullname)(.*)"

    var unresolved_sig_pattern = python_unresolved
    if (filename.equals(Constants.JAVA_STATS)) {
      unresolved_sig_pattern = java_unresolved_signature
    }

    xtocpg match {
      case Success(cpg) => {
        total = cpg.call.methodFullName.l.length
        unresolvedSignatures = cpg.call.methodFullName(unresolved_sig_pattern).l.length
        cpg.call.methodFullName(unresolved_sig_pattern).l.map(us => {
          unresolvedSignaturesList += us.methodFullName + "\n\t" + "Line Number: " + us.lineNumber.get + "\n\t" + "File: " + getFileNameForNode(us)
        })

        if (filename.equals(Constants.JAVA_STATS)) {
          unresolvedNamespaces = cpg.call.methodFullName(java_unresolved_namespace).l.length
          cpg.call.methodFullName(java_unresolved_namespace).l.map(un => {
            unresolvedNamespacesList += un.methodFullName + "\n\t" + "Line Number: " + un.lineNumber.get + "\n\t" + "File: " + getFileNameForNode(un)
          })
        }
      }
    }

    val statfile = File(filename)
    statfile.write("")
    val divider = "---------------------------------------------------------------------------------------------------------"

    var statstr = "\n" + divider + "\n"
    statstr += "Total number of function calls: " + total + "\n\n"

    var percentage: Double = 0.0

    statstr += "Calls with unresolved signatures: " + unresolvedSignatures + "\n"
    if (unresolvedSignatures > 0) {
      percentage = (unresolvedSignatures.toDouble * 100.0) / total.toDouble
      statstr += percentage + "% of total calls are unresolved" + "\n"
    }

    statstr += "\nCalls with unresolved namespace: " + unresolvedNamespaces + "\n"
    if (unresolvedNamespaces > 0) {
      percentage = (unresolvedNamespaces.toDouble * 100.0) / total.toDouble
      val subsetPercentage = (unresolvedNamespaces.toDouble * 100.0) / unresolvedSignatures.toDouble
      statstr += percentage + "% of total calls | " + subsetPercentage + "% of unresolved calls are unresolved namespaces" + "\n"
    }

    val resolved = total - unresolvedSignatures
    statstr += "\nResolved function calls: " + resolved + "\n"
    if (resolved > 0) {
      percentage = (resolved.toDouble * 100.0) / total.toDouble
      statstr += percentage + "% calls resolved" + "\n"
    }

    print(statstr)
    statfile.appendText(statstr)

    if (unresolvedSignaturesList.length > 0) {
      statfile.appendLine(divider)
      statfile.appendLine("List of Calls with Unresolved Signatures:")
      unresolvedSignaturesList.zipWithIndex.map { case (us, index) => statfile.appendLine((index + 1) + " - " + us) }
    }

    if (unresolvedNamespacesList.length > 0) {
      statfile.appendLine(divider)
      statfile.appendLine("List of Calls with Unresolved Namespaces:")
      unresolvedNamespacesList.zipWithIndex.map { case (un, index) => statfile.appendLine((index + 1) + " - " + un) }
    }

    println(divider)
    println()
  }
}
