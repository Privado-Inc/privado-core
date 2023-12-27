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
import ai.privado.languageEngine.ruby.passes.download.JRubyBasedParser
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
import org.jruby.ast.{ArrayNode, BlockNode, CallNode, FCallNode, IArgumentNode, IterNode, Node, StrNode}
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

class SchemaParser(cpg: Cpg, projectRoot: String, ruleCache: RuleCache) extends PrivadoParallelCpgPass[String](cpg) {

  val SCHEMA_FILE_PATTERN: String = ".*/schema.rb"
  val CREATE_TABLE: String        = "create_table"
  val ALL_OPERATOR: String        = "<operator>.*"

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[_ <: AnyRef] =
    SourceFiles
      .determine(projectRoot, Set(".rb"), ignoredFilesRegex = Option(ruleCache.getExclusionRegex.r))
      .filter(_.matches(SCHEMA_FILE_PATTERN))
      .toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode = addFileNode(file, builder)
    buildAndAddSqlQueryNodes(cpg, file, builder, fileNode)
  }

  private def buildAndAddSqlQueryNodes(
    cpg: Cpg,
    schemaFileName: String,
    builder: DiffGraphBuilder,
    fileNode: NewFile
  ): Unit = Try {

    val rootNode = JRubyBasedParser.parseFile(schemaFileName)

    var order: Int = 0
    rootNode.childNodes.forEach {
      case a: CallNode =>
        val bodyNode = a.getIterNode.asInstanceOf[IterNode].getBodyNode
        bodyNode match {
          case _: BlockNode => bodyNode.childNodes.forEach(processBodyNode(_, builder, fileNode, order))
          case _: FCallNode => processBodyNode(bodyNode, builder, fileNode, order)
        }
        order = order + 1
      case _ => logger.debug(s"Nothing matched for schema.rb file : $schemaFileName")
    }

  }
  @deprecated
  private def buildAndAddSqlQueryNodesUsingDefaultParser(
    cpg: Cpg,
    schemaFileName: String,
    builder: DiffGraphBuilder,
    fileNode: NewFile
  ): Unit = Try {

    var n: Int = 0
    cpg.call
      .name(CREATE_TABLE)
      .where(_.file.name(schemaFileName))
      .zipWithIndex
      .map((x, i) =>
        (
          x,
          x.astSiblings.isBlock.astChildren.isBlock
            .l(i)
            .ast
            .collectAll[io.shiftleft.codepropertygraph.generated.nodes.Call]
            .whereNot(_.isCallTo(ALL_OPERATOR))
            .astChildren
            .isLiteral
            .l
        )
      )
      .foreach(queryWthLine => {
        val callNode: Call         = queryWthLine._1
        val tableNode: Literal     = queryWthLine._1.astSiblings.isLiteral.dedup.l(n)
        val columns: List[Literal] = queryWthLine._2
        try {
          val sqlTable: SQLTable = SQLTable(
            tableNode.code.stripPrefix("\"").stripSuffix("\""),
            tableNode.lineNumber.getOrElse(Integer.valueOf(0)),
            tableNode.columnNumber.getOrElse(Integer.valueOf(0))
          )
          val sqlColumns: List[SQLColumn] = columns.map(x =>
            SQLColumn(
              x.code.stripPrefix("\"").stripSuffix("\""),
              x.lineNumber.getOrElse(Integer.valueOf(0)),
              x.columnNumber.getOrElse(Integer.valueOf(0))
            )
          )
          val queryModel = SQLQuery(SQLQueryType.CREATE, sqlTable, sqlColumns)

          SQLNodeBuilder.buildAndReturnIndividualQueryNode(
            builder,
            fileNode,
            queryModel,
            callNode.code,
            callNode.lineNumber.getOrElse(Integer.valueOf(0)),
            n
          )
          n = n + 1
        } catch {
          case ex: Exception =>
            ex.printStackTrace()
            println(s"Error while building schema.rb query at line $n: ${ex.getMessage}")
            None
        }
      })

  }

  private def processBodyNode(b: Node, builder: DiffGraphBuilder, fileNode: NewFile, order: Int): Unit = b match {
    case fCall: FCallNode if fCall.getName.toString == "create_table" =>
      try {
        getEntityNameForNode(fCall).foreach { table =>
          val tableNode = SQLTable(table._1, table._2, Constants.defaultLineNumber)
          val columns   = ListBuffer[SQLColumn]()

          fCall.getIterNode.asInstanceOf[IterNode].getBodyNode match {
            case b: BlockNode =>
              b.forEach {
                case c: CallNode if !List("index", "check_constraint").contains(c.getName.toString) =>
                  getEntityNameForNode(c).foreach { column =>
                    columns.addOne(SQLColumn(column._1, column._2, Constants.defaultLineNumber))
                  }
                case _ =>
              }
            case _ =>
          }

          val sqlQuery = SQLQuery(SQLQueryType.CREATE, tableNode, columns.toList)
          SQLNodeBuilder.buildAndReturnIndividualQueryNode(builder, fileNode, sqlQuery, "", tableNode.lineNumber, order)
        }
      } catch {
        case ex: Exception =>
          logger.debug(s"Error while building schema.rb, $ex")
          println(s"Error while building schema.rb query at line ${fCall.getLine}")
      }
    case _ =>

  }

  private def getEntityNameForNode(c: IArgumentNode) = {
    c.getArgsNode match {
      case argNode: ArrayNode if argNode.get(0).isInstanceOf[StrNode] =>
        val strNode    = argNode.get(0).asInstanceOf[StrNode]
        val entityName = strNode.getValue.toString
        val lineNumber = strNode.getLine + 1
        Some((entityName, lineNumber))
      case _ => None
    }
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }
}
