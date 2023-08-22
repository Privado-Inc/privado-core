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

import ai.privado.cache.RuleCache
import ai.privado.model.sql.{SQLColumn, SQLQuery, SQLQueryType, SQLTable}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.SQLParser.createSQLTableItem
import ai.privado.utility.{Utilities, SQLParser as UtilitySQLParser}
import better.files.*
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal, NewFile, NewSqlColumnNode, NewSqlQueryNode, NewSqlTableNode}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable
import scala.util.Try

class SchemaParser(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {

  val SCHEMA_FILE_PATTERN: String = ".*/schema.rb"

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[_ <: AnyRef] =
    cpg.file(SCHEMA_FILE_PATTERN).name.l.toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode = addFileNode(file, builder)
    buildAndAddSqlQueryNodes(cpg, file, builder, fileNode)
  }

  private def buildAndAddSqlQueryNodes(cpg: Cpg, schemaFileName: String, builder: DiffGraphBuilder, fileNode: NewFile): Unit = Try {

    var n : Int = 0
    cpg.call.name("create_table").where(_.file.name(SCHEMA_FILE_PATTERN)).zipWithIndex.map((x,i) => (x, x.astSiblings.isBlock.astChildren.isBlock.l(i).ast.collectAll[io.shiftleft.codepropertygraph.generated.nodes.Call].whereNot(_.isCallTo("<operator>.*")).astChildren.isLiteral.l))
      .foreach(queryWthLine => {
        val callNode: Call = queryWthLine._1
        val tableNode: Literal           = queryWthLine._1.astSiblings.isLiteral.dedup.l(n)
        val columns: List[Literal] = queryWthLine._2
        try {
          val table : SQLTable = SQLTable(tableNode.code, tableNode.lineNumber.getOrElse(Integer.valueOf(0)), tableNode.columnNumber.getOrElse(Integer.valueOf(0)))
          val sqlColumns : List[SQLColumn] = columns.map(x=>SQLColumn(x.code, x.lineNumber.getOrElse(Integer.valueOf(0)), x.columnNumber.getOrElse(Integer.valueOf(0))))
          val queryModel = SQLQuery(SQLQueryType.CREATE, table, sqlColumns)

          println(s"tableNode.code ${tableNode.code}")
          columns.foreach(x => println(x.code))
          buildAndReturnIndividualQueryNode(
            builder,
            fileNode,
            queryModel,
            callNode.code,
            callNode.lineNumber.getOrElse(Integer.valueOf(0)),
            n
          )
          n = n+1
        } catch {
          case ex: Exception =>
            ex.printStackTrace()
            println(s"Error while building schema.rb query at line $n: ${ex.getMessage}")
            None
        }
      })

  }

  private def buildAndReturnIndividualQueryNode(
                                                 builder: DiffGraphBuilder,
                                                 fileNode: NewFile,
                                                 queryModel: SQLQuery,
                                                 query: String,
                                                 queryLineNumber: Int,
                                                 queryOrder: Int
                                               ): Unit = {
    // Have added tableName in name key
    // Have added columns in value key

    val queryNode = NewSqlQueryNode().name(queryModel.queryType).code(query).lineNumber(queryLineNumber)

    val tableNode = NewSqlTableNode()
      .name(queryModel.table.name)
      .code(query)
      .lineNumber(queryLineNumber + queryModel.table.lineNumber - 1)
      .columnNumber(queryModel.table.columnNumber)
      .order(queryOrder)

    builder.addEdge(queryNode, tableNode, EdgeTypes.AST)
    builder.addEdge(queryNode, fileNode, EdgeTypes.SOURCE_FILE)
    builder.addEdge(tableNode, fileNode, EdgeTypes.SOURCE_FILE)

    queryModel.column.zipWithIndex.foreach { case (queryColumn: SQLColumn, columnIndex) =>
      val columnNode = NewSqlColumnNode()
        .name(queryColumn.name)
        .code(queryColumn.name)
        .lineNumber(queryLineNumber + queryColumn.lineNumber - 1)
        .columnNumber(queryColumn.columnNumber)
        .order(columnIndex)
      builder.addEdge(tableNode, columnNode, EdgeTypes.AST)
      builder.addEdge(columnNode, fileNode, EdgeTypes.SOURCE_FILE)
    }
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }
}
