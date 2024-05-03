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
import ai.privado.utility.{Utilities, SQLParser as UtilitySQLParser}
import ai.privado.utility.SQLNodeBuilder
import better.files.*
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewSqlColumnNode, NewSqlQueryNode, NewSqlTableNode}
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}
import better.files.File.VisitOptions
import org.jruby.ast.FileNode

import scala.collection.mutable
import scala.util.Try
class SQLParser(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[_ <: AnyRef] =
    getSQLFiles(projectRoot, Set(".sql")).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode = addFileNode(file, builder)
    buildAndAddSqlQueryNodes(file, builder, fileNode)
  }

  private def buildAndAddSqlQueryNodes(sqlFileName: String, builder: DiffGraphBuilder, fileNode: NewFile): Unit = Try {

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
          UtilitySQLParser.parseSqlQuery(query) match {
            case Some(parsedQueryList) =>
              parsedQueryList.zipWithIndex.foreach { case (parsedQueryItem: SQLQuery, queryOrder) =>
                SQLNodeBuilder.buildAndReturnIndividualQueryNode(
                  builder,
                  fileNode,
                  parsedQueryItem,
                  query,
                  queryLineNumber,
                  queryOrder,
                  fileName = Option(sqlFileName)
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
  private def getSQLFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    SourceFiles
      .determine(Set(projectRoot), extensions)(VisitOptions.default)
      .filter(Utilities.isFileProcessable(_, ruleCache))
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }
}
