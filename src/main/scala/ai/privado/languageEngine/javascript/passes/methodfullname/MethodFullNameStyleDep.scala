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

package ai.privado.languageEngine.javascript.passes.methodfullname

import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters.CollectionHasAsScala

class MethodFullNameStyleDep(cpg: Cpg) extends ForkJoinParallelCpgPass[(String, String, String, String)](cpg) {

  val cachedCall         = cpg.call.whereNot(_.name(Operators.ALL.asScala.toSeq: _*)).l
  val cachedOperatorCall = cpg.call(Operators.assignment).l

  /** Processes nodes and return metaInformation needed for tagging call nodes with methodFullName
    * @return
    *   \- (importedAs, importedEntity, fileName, packageName) importedAs - name/function which was imported from
    *   dependency importedEntity - Actual dependency name filename - the file where the import happened packageName -
    *   prefix name to be added in methodFullName
    */
  override def generateParts(): Array[(String, String, String, String)] = {
    println("inside MethodFullNameStyleDep - importStyleDependency started")
    // Captures `import * as cors from 'cors'` style
    val importStyleDependency = cpg.imports
      .map(staticImport =>
        (
          staticImport.importedAs.getOrElse(""),
          staticImport.importedEntity.getOrElse(""),
          staticImport.file.name.headOption.getOrElse(""),
          "pkg."
        )
      )
      .toArray
    println("inside MethodFullNameStyleDep - importStyleDependency done")
    // Captures `console` node
    val consoleNodes = cpg.file.map(fileName => ("console", "console", fileName.name, "pkg.")).toArray
    println("inside MethodFullNameStyleDep - consoleNodes done")
    importStyleDependency ++ consoleNodes
  }

  override def runOnPart(builder: DiffGraphBuilder, importedTuple: (String, String, String, String)): Unit = {

    val importedAs     = importedTuple._1
    val importedEntity = importedTuple._2
    val fileName       = importedTuple._3
    val packageName    = importedTuple._4

    // dependency directly consumed as call node `cors()`
    cachedCall
      .filter(_.file.name.headOption.getOrElse("").equals(fileName))
      .filter(_.name.equals(importedAs))
      .foreach(callNode => updateCallNode(builder, callNode, importedEntity, packageName))

    // dependency consumed via a identifier node `bodyParser.verifyJson()`
    cpg
      .identifier(importedAs)
      .filter(_.file.name.headOption.getOrElse("").equals(fileName))
      .astParent
      .isCall
      .nameNot(Operators.ALL.asScala.toSeq: _*)
      .foreach(callNode => updateCallNode(builder, callNode, importedEntity, packageName))

  }

  /** Adds methodFullName to call nodes with the information passed
    * @param builder
    *   \- DiffGraphBuilder object
    * @param callNode
    *   \- node on which methodFullName needs to be updated
    * @param importedEntity
    *   \- library name
    * @param defaultPackageName
    *   \- prefix for methodFullName
    * @return
    */
  private def updateCallNode(
    builder: DiffGraphBuilder,
    callNode: Call,
    importedEntity: String,
    defaultPackageName: String = ""
  ) = {

    // To not update node if call name is --> then
    if (!callNode.name.equals("then")) {
      builder.setNodeProperty(
        callNode,
        PropertyNames.MethodFullName,
        defaultPackageName + importedEntity + "." + callNode.name
      )
    }
  }
}
