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

import ai.privado.utility.{SQLParser, Utilities}
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewSqlQueryNode}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.collection.mutable
import scala.io.Source
class SQLParser(cpg: Cpg, projectRoot: String) extends ForkJoinParallelCpgPass[String](cpg) {

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[_ <: AnyRef] =
    getSQLFiles(projectRoot, Set(".sql")).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode      = addFileNode(file, builder)
    val sqlQueryNodes = getSqlQueryNodes(file, builder)
    sqlQueryNodes.foreach(builder.addEdge(_, fileNode, EdgeTypes.SOURCE_FILE))

  }

  def getSqlQueryNodes(sqlFileName: String, builder: DiffGraphBuilder) = {

    val sqlFile      = Source.fromFile(sqlFileName)
    var lineNumber   = 0
    var queryLen     = 0
    var queryBuilder = new StringBuilder()
    val sqlQueries   = mutable.ListBuffer[(String, Int)]()
    for (line <- sqlFile.getLines()) {
      lineNumber += 1

      if (line.trim().nonEmpty && !line.trim().startsWith("-")) {
        queryBuilder.append(line)
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
    sqlFile.close()

    sqlQueries
      .flatMap(queryWthLine => {

        val query           = queryWthLine._1
        val queryLineNumber = queryWthLine._2
        SQLParser.parseSqlQuery(query) match {
          case Some((queryName, tableName, columns)) =>
            // Have added tableName in name key
            // Have added columns in value key
            // findMatchingIndices(lines, query).headOption.getOrElse(-1)
            val sqlQueryNode =
              NewSqlQueryNode()
                .code(query)
                .name(tableName)
                .fullName(query)
                .value(columns.mkString(","))
                .lineNumber(queryLineNumber)
            builder.addNode(sqlQueryNode)
            Some(sqlQueryNode)
          case None => None
        }
      })
      .toList

  }

  private def getSQLFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    SourceFiles
      .determine(Set(projectRoot), extensions)
      .filter(Utilities.isFileProcessable)
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }
}
