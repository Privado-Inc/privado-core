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

import ai.privado.model.Constants
import ai.privado.model.Constants.{defaultLineNumber}
import ai.privado.model.sql.{SQLColumn, SQLQuery, SQLQueryType, SQLTable}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.SQLNodeBuilder
import better.files.*
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}

import scala.util.{Failure, Success, Try}

abstract class BaseORMParser(cpg: Cpg) extends PrivadoParallelCpgPass[TypeDecl](cpg) {

  override def generateParts(): Array[_ <: AnyRef] = ???
  val logger                                       = LoggerFactory.getLogger(getClass)

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
        model.name,
        model.lineNumber.getOrElse(Integer.valueOf(defaultLineNumber)),
        model.columnNumber.getOrElse(Integer.valueOf(defaultLineNumber))
      )
      val sqlColumns: List[SQLColumn] = model.member.l.map(member =>
        SQLColumn(
          member.name,
          member.lineNumber.getOrElse(Integer.valueOf(defaultLineNumber)),
          member.columnNumber.getOrElse(Integer.valueOf(defaultLineNumber))
        )
      )
      val queryModel = SQLQuery(SQLQueryType.CREATE, sqlTable, sqlColumns)
      SQLNodeBuilder.buildAndReturnIndividualQueryNode(
        builder,
        fileNode,
        queryModel,
        model.code,
        model.lineNumber.getOrElse(Integer.valueOf(defaultLineNumber)),
        0
      )
    } catch {
      case ex: Exception =>
        logger.error(s"Error while building SQL nodes: ${ex.getMessage}")
        logger.debug(s"""Error while building SQL nodes for
             |TypeDecl: name ${model.name} \n
             |code: ${model.code} \n
             |file: ${model.file.head.name}""".stripMargin)
    }
  }
}
