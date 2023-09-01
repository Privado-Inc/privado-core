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

import better.files.File
import io.joern.rubysrc2cpg.utils.{MethodTableModel, PackageTable}
import io.joern.x2cpg.passes.frontend.ImportsPass.*
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

import java.io.File as JFile
import java.util.regex.{Matcher, Pattern}
import scala.collection.Seq
class RubyImportResolverPass(cpg: Cpg, packageTableInfo: PackageTable) extends XImportResolverPass(cpg) {

  private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {

    resolveEntities(importedEntity, importCall, fileName).foreach(x => resolvedImportToTag(x, importCall, diffGraph))
  }

  private def resolveEntities(expEntity: String, importCall: Call, fileName: String): Set[ResolvedImport] = {

    // TODO
    /* Currently we are considering only case where exposed module are Classes,
    and the only way to consume them is by creating a new object as we encounter more cases,
     This needs to be handled accordingly
     */

    val expResolvedPath =
      if (packageTableInfo.getModule(expEntity).nonEmpty || packageTableInfo.getTypeDecl(expEntity).nonEmpty)
        expEntity
      else if (expEntity.contains("."))
        getResolvedPath(expEntity, fileName)
      else if (cpg.file.name(s".*$expEntity.rb").nonEmpty)
        getResolvedPath(s"$expEntity.rb", fileName)
      else
        expEntity

    // TODO Limited ResolvedMethod exposure for now, will open up after looking at more concrete examples
    val finalResolved = {
      if (
        packageTableInfo.getModule(expResolvedPath).nonEmpty || packageTableInfo.getTypeDecl(expResolvedPath).nonEmpty
      ) {
        val importNodesFromTypeDecl = packageTableInfo
          .getTypeDecl(expEntity)
          .flatMap { typeDeclModel =>
            Seq(
              ResolvedMethod(s"${typeDeclModel.fullName}.new", "new"),
              ResolvedMethod(s"${typeDeclModel.fullName}.${typeDeclModel.name}", typeDeclModel.name),
              ResolvedTypeDecl(typeDeclModel.fullName)
            )
          }
          .distinct

        val importNodesFromModule = packageTableInfo.getModule(expEntity).flatMap { moduleModel =>
          Seq(ResolvedTypeDecl(moduleModel.fullName))
        }
        (importNodesFromTypeDecl ++ importNodesFromModule).toSet
      } else {
        val resolvedTypeDecls = cpg.typeDecl
          .where(_.file.name(s"${Pattern.quote(expResolvedPath)}\\.?.*"))
          .flatMap(typeDecl =>
            Seq(
              ResolvedTypeDecl(typeDecl.fullName),
              ResolvedMethod(s"${typeDecl.fullName}.new", "new"),
              ResolvedMethod(s"${typeDecl.fullName}.${typeDecl.name}", typeDecl.name)
            )
          )
          .toSet

        val resolvedModules = cpg.namespaceBlock
          .whereNot(_.nameExact("<global>"))
          .where(_.file.name(s"${Pattern.quote(expResolvedPath)}\\.?.*"))
          .flatMap(module => Seq(ResolvedTypeDecl(module.fullName)))
          .toSet

        // Expose methods which are directly present in a file, without any module, TypeDecl
        val resolvedMethods = cpg.method
          .where(_.file.name(s"${Pattern.quote(expResolvedPath)}\\.?.*"))
          .where(_.nameExact(":program"))
          .astChildren
          .astChildren
          .isMethod
          .flatMap(method => Seq(ResolvedMethod(method.fullName, method.name)))
          .toSet
        resolvedTypeDecls ++ resolvedModules ++ resolvedMethods
      }
    }

    finalResolved
  }

  def getResolvedPath(expEntity: String, fileName: String) = {
    val rawEntity   = expEntity.stripPrefix("./")
    val matcher     = pathPattern.matcher(rawEntity)
    val sep         = Matcher.quoteReplacement(JFile.separator)
    val root        = s"$codeRoot${JFile.separator}"
    val currentFile = s"$root$fileName"
    val entity      = if (matcher.find()) matcher.group(1) else rawEntity
    val resolvedPath = better.files
      .File(
        currentFile.stripSuffix(currentFile.split(sep).lastOption.getOrElse("")),
        entity.split("\\.").headOption.getOrElse(entity)
      )
      .pathAsString match {
      case resPath if entity.endsWith(".rb") => s"$resPath.rb"
      case resPath                           => resPath
    }
    resolvedPath
  }

}
