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

package ai.privado.passes

import ai.privado.cache.RuleCache
import ai.privado.model.sql.{SQLColumn, SQLQuery}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.{SQLParser, Utilities}
import better.files._
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewSqlColumnNode, NewSqlQueryNode, NewSqlTableNode}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.collection.mutable
class SQLParser(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[_ <: AnyRef] =
    getSQLFiles(projectRoot, Set(".sql")).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode = addFileNode(file, builder)
    buildAndAddSqlQueryNodes(file, builder, fileNode)
  }

  private def buildAndAddSqlQueryNodes(sqlFileName: String, builder: DiffGraphBuilder, fileNode: NewFile): Unit = {

    val sqlFile      = File(sqlFileName)
    var lineNumber   = 0
    var queryLen     = 0
    var queryBuilder = new StringBuilder()
    val sqlQueries   = mutable.ListBuffer[(String, Int)]()
    for (line <- sqlFile.lines) {
      lineNumber += 1

      if (line.trim().nonEmpty && !line.trim().startsWith("--")) {
        queryBuilder.append(line + "\n")
        queryLen += 1

        if (line.trim().endsWith(";")) {
          try {
            val query = queryBuilder.toString().trim()
            queryBuilder = new StringBuilder()
            sqlQueries.addOne(query, lineNumber - queryLen + 1)
            queryLen = 0
          } catch {
            case e: Exception =>
              logger.debug(s"Error on line $lineNumber: ${e.getMessage}")
          }
        }
      }
    }

    sqlQueries
      .foreach(queryWthLine => {
        val query           = queryWthLine._1
        val queryLineNumber = queryWthLine._2
        try {
          SQLParser.parseSqlQuery(query) match {
            case Some(parsedQueryList) =>
              parsedQueryList.zipWithIndex.foreach { case (parsedQueryItem: SQLQuery, queryOrder) =>
                buildAndReturnIndividualQueryNode(
                  builder,
                  fileNode,
                  parsedQueryItem,
                  query,
                  queryLineNumber,
                  queryOrder
                )
              }
            case None =>
              logger.debug("Failed to parse: ", sqlFileName, " : ", queryLineNumber)
              None
          }
        } catch {
          case ex: Exception =>
            println(s"Error while parsing SQL query at line $queryLineNumber: ${ex.getMessage}")
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

  private def getSQLFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    SourceFiles
      .determine(Set(projectRoot), extensions)
      .filter(Utilities.isFileProcessable(_, ruleCache))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }
}
