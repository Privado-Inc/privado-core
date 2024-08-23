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

import ai.privado.tagger.PrivadoSimpleCpgPass
import io.joern.x2cpg.Ast
import io.joern.x2cpg.passes.frontend.{CallAlias, LocalKey, LocalVar, SymbolTable, XImportsPass}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.Imports.createImportNodeAndLink
import io.shiftleft.semanticcpg.language.importresolver.{ResolvedMethod, ResolvedTypeDecl}
import io.shiftleft.codepropertygraph.generated.nodes.{File, NewCall, NewFile}
import io.shiftleft.semanticcpg.language.*

import scala.collection.Seq

class GlobalImportPass(cpg: Cpg, globalSymbolTable: SymbolTable[LocalKey]) extends PrivadoSimpleCpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit = {

    val resolvedTypeDeclInternal = cpg.typeDecl
      .filterNot(_.astParent.isNamespaceBlock)
      .flatMap(typeDecl => Seq(ResolvedTypeDecl(typeDecl.fullName)))
      .l

    val resolvedModuleInternal = cpg.typeDecl
      .filter(_.astParent.isNamespaceBlock)
      .filter(_.astChildren.size != 1)
      .flatMap(typeDecl => Seq(ResolvedTypeDecl(typeDecl.fullName)))
      .l

    (resolvedTypeDeclInternal ++ resolvedModuleInternal).toSet
      .foreach { case ResolvedTypeDecl(fullName, _) =>
        globalSymbolTable.append(LocalVar(fullName.split("\\.").lastOption.getOrElse(fullName)), fullName)
      }
  }
}
