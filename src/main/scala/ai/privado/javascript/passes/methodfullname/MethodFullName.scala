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

package ai.privado.javascript.passes.methodfullname

import ai.privado.semantic.Language.finder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class MethodFullName(cpg: Cpg) extends ConcurrentWriterCpgPass[(String, String, String)](cpg) {

  val cachedCall         = cpg.call.whereNot(_.name(".*operator.*")).l
  val cachedOperatorCall = cpg.call("<operator>.assignment").l
  override def generateParts(): Array[(String, String, String)] = {

    val requireStyleDependency = cpg.dependency.name
      .flatMap(dependencyName => {
        Traversal(cachedOperatorCall)
          .where(
            _.argument(2)
              .code(".*require.*('" + dependencyName + "'|\"" + dependencyName + "\").*")
          )
          .argument(1)
          .isIdentifier
          .map(item => (item.name, dependencyName, item.location.filename))
      })
      .toArray

    val importStyleDependency = cpg.staticImport
      .map(staticImport =>
        (staticImport.importedAs.getOrElse(""), staticImport.importedEntity.getOrElse(""), staticImport.file.head.name)
      )
      .toArray
    importStyleDependency ++ requireStyleDependency
  }

  override def runOnPart(builder: DiffGraphBuilder, importedTuple: (String, String, String)): Unit = {
    val importedAs     = importedTuple._1
    val importedEntity = importedTuple._2
    val fileName       = importedTuple._3

    cachedCall
      .filter(_.name.equals(importedAs))
      .filter(_.location.filename.equals(fileName))
      .foreach(callNode => updateCallNode(builder, callNode, importedAs, importedEntity))

    cpg
      .identifier(importedAs)
      .filter(_.location.filename.equals(fileName))
      .astParent
      .isCall
      .nameNot(".*operator.*")
      .foreach(callNode => updateCallNode(builder, callNode, importedAs, importedEntity))

  }

  private def updateCallNode(builder: DiffGraphBuilder, callNode: Call, importedAs: String, importedEntity: String) = {
    builder.setNodeProperty(callNode, PropertyNames.MethodFullName, "pkg." + importedEntity + "." + callNode.name)
  }
}
