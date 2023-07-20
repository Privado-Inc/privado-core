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

import scala.util.{Failure, Success, Try}
import ai.privado.model.Constants
import ai.privado.model.Language

import scala.collection.mutable.ListBuffer
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.utility.Utilities.{getFileNameForNode, resolver}
import io.shiftleft.codepropertygraph.generated.nodes.Call

object UnresolvedReportUtility {
  def reportUnresolvedMethods(xtocpg: Try[Cpg], statoutdir: String, language: Language.Language): Unit = {
    var total                         = 0
    var unresolvedSignatures          = 0
    var unresolvedNamespaces          = 0
    var unresolvedSignatureWithCallee = 0
    var unresolvedSignaturesList      = ListBuffer[String]()
    var unresolvedNamespacesList      = ListBuffer[String]()
    var nonempty                      = 0
    var isempty                       = 0
    var resolvedCalls                 = 0

    val unresolved_signature = "(?i)(.*)(unresolved)(signature)(.*)"
    val unresolved_namespace = "(?i)(.*)(unresolved)(namespace)(.*)"
    var unknown_full_name    = "(?i)(.*)(unknownfullname|empty)(.*)"
    val operator_name        = "(?i)(operator).*"
    val importRegex          = "(?i).*(import|require).*"

    if (language.equals(Language.RUBY)) {
      unknown_full_name = "(?i)(.*)(unknownfullname|empty)(.*)"
    }

    var unresolved_sig_pattern = unknown_full_name
    if (language.equals(Language.JAVA)) {
      unresolved_sig_pattern = unresolved_signature
    }

    val resolvedCallMethodFullNames = ListBuffer[Call]()
    xtocpg match {
      case Success(cpg) => {
        val importCount = cpg.call.or(_.filter((i) => {
            i.name.matches(importRegex)
          }), _.where(_.code("<empty>"))).size

        val operatorCount = cpg.call.count(i => !i.name.matches(operator_name))
        total = cpg.call.methodFullName.size - importCount - operatorCount
        unresolvedSignatures = cpg.call.methodFullName(unresolved_sig_pattern).size

        nonempty = cpg.call.callee.filter(_.nonEmpty == false).l.length
        isempty = cpg.call.callee.filter(_.isEmpty).l.length

        // Resolved calls list
        resolvedCalls = resolvedCallMethodFullNames
          .addAll(cpg.call.whereNot(_.methodFullName(s"$unknown_full_name|$operator_name|$importRegex")).l)
          .length

        cpg.call
          .methodFullName(unresolved_sig_pattern)
          .l
          .filter((i) => {
            var res = true
            if (language.equals(Language.PYTHON)) {
              res = i.name != "import"
            }
            res
          })
          .map(us => {
            if (us.callee.fullName.l.length > 0) {
              unresolvedSignatureWithCallee += 1
            }
            unresolvedSignaturesList += us.methodFullName + "(" + us.name + ")" + "\n\t" + "Line Number: " + us.lineNumber.get + "\n\t" + "File: " + getFileNameForNode(
              us
            ) + "\n\t" + "Callee FullName: " + us.callee.fullName.l
          })

        if (language.equals(Language.JAVA)) {
          unresolvedNamespaces = cpg.call.methodFullName(unresolved_namespace).l.length
          cpg.call
            .methodFullName(unresolved_namespace)
            .l
            .map(un => {
              unresolvedNamespacesList += un.methodFullName + "\n\t" + "Line Number: " + un.lineNumber.get + "\n\t" + "File: " + getFileNameForNode(
                un
              )
            })
        }
      }
      case Failure(_) => None
    }

    val outputDirectory = File(statoutdir).createDirectoryIfNotExists()
    var statfilepath    = ""
    language match {
      case Language.JAVA       => statfilepath = s"$outputDirectory/${Constants.JAVA_STATS}"
      case Language.JAVASCRIPT => statfilepath = s"$outputDirectory/${Constants.JS_STATS}"
      case Language.PYTHON     => statfilepath = s"$outputDirectory/${Constants.PYTHON_STATS}"
      case Language.RUBY       => statfilepath = s"$outputDirectory/${Constants.RUBY_STATS}"
    }
    val statfile = File(statfilepath)
    statfile.write("")
    val divider =
      "---------------------------------------------------------------------------------------------------------"

    var statstr = s"\n$divider\n"
    statstr += s"Total number of function calls: $total\n\n"

    var percentage: Double = 0.0

    statstr += s"Calls with unresolved signatures: $unresolvedSignatures\n"
    if (unresolvedSignatures > 0) {
      percentage = (unresolvedSignatures.toDouble * 100.0) / total.toDouble
      statstr += s"$percentage% of total calls are unresolved\n"
    }

    if (language.equals(Language.PYTHON)) {
      statstr += s"\nCalls with unresolved signatures having callee: $unresolvedSignatureWithCallee\n"
      if (unresolvedSignatureWithCallee > 0) {
        percentage = (unresolvedSignatureWithCallee.toDouble * 100.0) / unresolvedSignatures.toDouble
        statstr += s"$percentage% of unresolved signatures having callee from unresolved signatures\n"
      }
    }

    statstr += s"\nCalls with unresolved namespace: $unresolvedNamespaces\n"
    if (unresolvedNamespaces > 0) {
      percentage = (unresolvedNamespaces.toDouble * 100.0) / total.toDouble
      val subsetPercentage = (unresolvedNamespaces.toDouble * 100.0) / unresolvedSignatures.toDouble
      statstr += s"$percentage% of total calls | $subsetPercentage% of unresolved calls are unresolved namespaces\n"
    }

    val resolved = total - unresolvedSignatures
    statstr += s"\nResolved function calls: $resolved\n"
    if (resolved > 0) {
      percentage = (resolved.toDouble * 100.0) / total.toDouble
      statstr += s"$percentage% calls resolved\n"
    }

//    if (nonempty > 0)
    statstr += s"\nCalls with nonEmpty Callee false: $nonempty"
//    if (isempty > 0)
    statstr += s"\nCalls with isEmpty Callee: $isempty\n"

    if (unresolvedSignaturesList.length > 0) {
      statfile.appendLine(divider)
      statfile.appendLine("List of Calls with Unresolved Signatures:")
      unresolvedSignaturesList.zipWithIndex.map { case (us, index) => statfile.appendLine(s"${(index + 1)} - $us") }
    }

    if (unresolvedNamespacesList.length > 0) {
      statfile.appendLine(divider)
      statfile.appendLine("List of Calls with Unresolved Namespaces:")
      unresolvedNamespacesList.zipWithIndex.map { case (un, index) => statfile.appendLine(s"${(index + 1)} - $un") }
    }

    statstr += s"$divider\n"
    statstr += "Resolved calls: \n"

    statfile.appendLine(divider)
    statfile.appendLine("Resolved calls: \n")

    resolvedCallMethodFullNames.foreach(call => {
      statfile.appendLine(call.methodFullName)
      statstr += s"${call.methodFullName}\n"
    })

    print(statstr)
    statfile.appendText(statstr)

    println(divider)
    println()
  }
}
