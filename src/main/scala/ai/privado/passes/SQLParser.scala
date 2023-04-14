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
import io.shiftleft.utils.IOUtils
import org.jooq.SQLDialect
import org.jooq.impl.DSL
import overflowdb.BatchedUpdate

import java.nio.file.Paths
import scala.io.Source
import scala.jdk.CollectionConverters._
class SQLParser(cpg: Cpg, projectRoot: String) extends ForkJoinParallelCpgPass[String](cpg) {
  override def generateParts(): Array[_ <: AnyRef] =
    getSQLFiles(projectRoot, Set(".sql")).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode      = addFileNode(file, builder)
    val sqlQueryNodes = getSqlQueryNodes(file, builder)
    sqlQueryNodes.foreach(builder.addEdge(_, fileNode, EdgeTypes.SOURCE_FILE))

  }

  def getSqlQueryNodes(sqlFileName: String, builder: DiffGraphBuilder) = {

    val sqlFile          = Source.fromFile(sqlFileName)
    val parsedStatements = DSL.using(SQLDialect.DEFAULT).parser.parse(sqlFile.mkString).asScala.toList
    val queries          = parsedStatements.collect { case q: org.jooq.Query => q }
    val sqlQueries       = queries.map(_.getSQL())

    sqlFile.close()
    val lines = IOUtils.readLinesInFile(Paths.get(sqlFileName)).toList

    sqlQueries.flatMap(query => {
      SQLParser.parseSqlQuery(query) match {
        case Some((queryName, tableName, columns)) =>
          // Have added tableName in name key
          // Have added columns in value key
          val fileLineNumber = -1
          // findMatchingIndices(lines, query).headOption.getOrElse(-1)
          val sqlQueryNode =
            NewSqlQueryNode().name(tableName).fullName(query).value(columns.mkString(",")).lineNumber(fileLineNumber)
          builder.addNode(sqlQueryNode)
          Some(sqlQueryNode)
        case None => None
      }
    })

  }

  def findMatchingIndices(strings: List[String], search: String): List[Int] = {
    strings.zipWithIndex
      .filter { case (string, index) =>
        search == string
      }
      .map { case (string, index) => index }
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
