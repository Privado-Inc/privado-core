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
import ai.privado.model.Constants.defaultLineNumber
import ai.privado.model.sql.{SQLColumn, SQLQuery, SQLQueryType, SQLTable}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.SQLNodeBuilder
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}

import scala.util.{Failure, Success, Try}

class ORMParserPass(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[TypeDecl](cpg) {

  val logger = LoggerFactory.getLogger(getClass)
  // when we pass a object's variable as its address its written as like `&users`
  // In joern its signatured as *[]packagename.User
  val ADDRESS_OF_OBJECT_SYMBOL = "*[]"
  override def generateParts(): Array[_ <: AnyRef] = {
    val storageRule = ruleCache.getRule.sinks
      .filter(rule => rule.id.matches("Storages.*"))
      .map(_.combinedRulePattern)
      .mkString("|")
    val arguments = cpg.call
      .methodFullName(storageRule)
      .argument
      .l
    val typeFullNames = arguments.isCall.typeFullName
      .map(x => x.stripPrefix(ADDRESS_OF_OBJECT_SYMBOL))
      .map(x => x.stripPrefix("*"))
      .dedup
      .l ++ arguments.isIdentifier.typeFullName
      .map(x => x.stripPrefix(ADDRESS_OF_OBJECT_SYMBOL))
      .map(x => x.stripPrefix("*"))
      .dedup
      .l
    val typeFullNameRegex = typeFullNames.mkString("|").replace("[]", "")
    cpg.typeDecl.fullName(typeFullNameRegex).dedup.toArray
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

  def buildAndAddSqlQueryNodes(
    builder: DiffGraphBuilder,
    typeDeclNode: TypeDecl,
    fileNode: NodeOrDetachedNode
  ): Unit = {
    try {
      val sqlTable: SQLTable = SQLTable(
        typeDeclNode.name,
        typeDeclNode.lineNumber.getOrElse(Integer.valueOf(defaultLineNumber)),
        typeDeclNode.columnNumber.getOrElse(Integer.valueOf(defaultLineNumber))
      )
      val sqlColumns: List[SQLColumn] = typeDeclNode.member.l.map(member =>
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
        typeDeclNode.code,
        typeDeclNode.lineNumber.getOrElse(Integer.valueOf(defaultLineNumber)),
        0
      )
    } catch {
      case ex: Exception =>
        logger.error(s"Error while building SQL nodes: ${ex.getMessage}")
        logger.debug(s"""Error while building SQL nodes for
             |TypeDecl: name ${typeDeclNode.name} \n
             |code: ${typeDeclNode.code} \n
             |file: ${typeDeclNode.file.headOption.getOrElse("unknown file")}""".stripMargin)
    }
  }
}
