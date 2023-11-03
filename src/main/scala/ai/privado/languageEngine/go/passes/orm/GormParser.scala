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
package ai.privado.languageEngine.go.passes.orm

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.model.sql.{SQLColumn, SQLQuery, SQLQueryType, SQLTable}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.SQLNodeBuilder
import ai.privado.utility.SQLParser.createSQLTableItem
import better.files.*
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class GormParser(cpg: Cpg) extends PrivadoParallelCpgPass[TypeDecl](cpg) {
  val GORM_PARAMETER_TYPE_RULE = ".*github.com/jinzhu/gorm.*"
  val logger                   = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.typeDecl.where(_.method.parameter.typeFullName(GORM_PARAMETER_TYPE_RULE)).dedup.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, model: TypeDecl): Unit = {
    Try(model.file.head) match {
      case Success(fileNode) =>
        buildAndAddSqlQueryNodes(builder, model, fileNode)
      case Failure(_) =>
        val fileNode = NewFile().name(Constants.Unknown)
        buildAndAddSqlQueryNodes(builder, model, fileNode)
    }
  }

  private def buildAndAddSqlQueryNodes(
    builder: DiffGraphBuilder,
    model: TypeDecl,
    fileNode: NodeOrDetachedNode
  ): Unit = {
    try {
      val sqlTable: SQLTable = SQLTable(
        model.code,
        model.lineNumber.getOrElse(Integer.valueOf(-1)),
        model.columnNumber.getOrElse(Integer.valueOf(-1))
      )
      val sqlColumns: List[SQLColumn] = model.member.l.map(x =>
        SQLColumn(x.code, x.lineNumber.getOrElse(Integer.valueOf(-1)), x.columnNumber.getOrElse(Integer.valueOf(-1)))
      )
      val queryModel = SQLQuery(SQLQueryType.CREATE, sqlTable, sqlColumns)
      SQLNodeBuilder.buildAndReturnIndividualQueryNode(
        builder,
        fileNode,
        queryModel,
        model.code,
        model.lineNumber.getOrElse(Integer.valueOf(-1)),
        0
      )

    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        println(s"Error while building sql nodes for GORM framework: ${ex.getMessage}")
    }
  }
}
