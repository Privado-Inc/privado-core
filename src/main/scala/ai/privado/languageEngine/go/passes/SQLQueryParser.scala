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

package ai.privado.languageEngine.go.passes

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.go.passes.orm.ORMParserPass
import ai.privado.model.Constants
import ai.privado.model.Constants.defaultLineNumber
import ai.privado.model.sql.{SQLColumn, SQLQuery}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.{SQLNodeBuilder, Utilities, SQLParser as UtilitySQLParser}
import better.files.*
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  Literal,
  NewFile,
  NewSqlColumnNode,
  NewSqlQueryNode,
  NewSqlTableNode
}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class SQLQueryParser(cpg: Cpg) extends PrivadoParallelCpgPass[AstNode](cpg) {

  val sqlQueryRegexPattern = "(?i).*(SELECT|INSERT|UPDATE|DELETE|CREATE|ALTER|DROP|TRUNCATE).*"
  val logger               = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[_ <: AnyRef] = {
    //    CPG query to fetch the Literal with SQL string
    //    'Repeat until' is used to combine multiline SQL queries into one
    cpg.literal
      .code(sqlQueryRegexPattern)
      .repeat(_.astParent)(_.until(_.isCall.whereNot(_.name(s"${Operators.addition}|<operator>.stringExpressionList"))))
      .isCall
      .argument
      .code(sqlQueryRegexPattern)
      .toArray

  }

  def runOnPart(builder: DiffGraphBuilder, node: AstNode): Unit = {
    Try(node.file.head) match {
      case Success(fileNode) =>
        buildAndAddSqlQueryNodes(builder, node, fileNode)
      case Failure(_) =>
        val fileNode = NewFile().name(Constants.Unknown)
        buildAndAddSqlQueryNodes(builder, node, fileNode)
    }
  }

  def buildAndAddSqlQueryNodes(builder: DiffGraphBuilder, node: AstNode, fileNode: NodeOrDetachedNode): Unit = Try {

    val queryLineNumber = node.lineNumber.getOrElse(Integer.valueOf(defaultLineNumber))
    val query           = node.code.stripPrefix("`").stripSuffix("`").stripPrefix("\"").stripSuffix("\"")
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
              queryOrder
            )
          }
        case None =>
          logger.debug("Failed to parse: ", query, " : ", queryLineNumber)
          None
      }
    } catch {
      case ex: Exception =>
        logger.debug(
          s"Error while parsing SQL query at line $queryLineNumber: ${ex.getMessage}" +
            s"query : ${node}"
        )
        None
    }

  }
}
