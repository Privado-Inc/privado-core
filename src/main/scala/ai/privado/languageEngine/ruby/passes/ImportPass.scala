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

package ai.privado.languageEngine.ruby.passes

import io.joern.x2cpg.Ast
import io.joern.x2cpg.passes.frontend.XImportsPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.shiftleft.codepropertygraph.generated.nodes.{File, NewCall, NewFile}
import io.shiftleft.semanticcpg.language.*

class ImportPass(cpg: Cpg, packageTable: PackageTable) extends ConcurrentWriterCpgPass[File](cpg) {

  lazy val modules: Set[String] =
    (packageTable.moduleMapping.keys.l ++ packageTable.typeDeclMapping.keys.l).toSet.filter(_.nonEmpty)
  def generateParts(): Array[File] = {
    cpg.file.name(".*[.]rb").toArray
  }

  def runOnPart(builder: DiffGraphBuilder, fileNode: File): Unit = {

    modules.foreach { moduleKey =>
      if (!fileNode.name.equals(moduleKey)) {
        val callNode   = NewCall().name(moduleKey)
        val importNode = createImportNodeAndLink(moduleKey, "", Some(callNode), builder)

        builder.addEdge(fileNode, callNode, EdgeTypes.AST)
        val callAst = Ast(callNode).withChild(Ast(importNode))
        storeInDiffGraph(callAst, builder)
      }
    }
  }
}
