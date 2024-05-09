package ai.privado.utility

import ai.privado.model.sql.{SQLColumn, SQLQuery, SQLQueryType, SQLTable}
import ai.privado.utility.SQLParser.{logger, parseSqlQuery}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewSqlColumnNode, NewSqlQueryNode, NewSqlTableNode}

import java.io.StringReader
import scala.util.matching.Regex
import net.sf.jsqlparser.JSQLParserException
import net.sf.jsqlparser.expression.{BinaryExpression, CastExpression, Expression, Function}
import net.sf.jsqlparser.parser.{ASTNodeAccess, CCJSqlParserUtil}
import net.sf.jsqlparser.schema.Table
import net.sf.jsqlparser.statement.Statement
import net.sf.jsqlparser.statement.create.table.CreateTable
import net.sf.jsqlparser.statement.insert.Insert
import net.sf.jsqlparser.statement.select.{PlainSelect, Select, SelectItem, SetOperationList, ParenthesedSelect}
import net.sf.jsqlparser.statement.update.Update
import org.slf4j.{Logger, LoggerFactory}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import overflowdb.NodeOrDetachedNode

import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.control.Breaks.{break, breakable}
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}

object SqlCleaner {
  def clean(sql: String): String = {
    var cleanedSql = removeComments(sql)
    cleanedSql = removeDynamicVariables(cleanedSql)
    cleanedSql.replace("`", "")
  }

  private def removeComments(sql: String): String = {
    // Replace /* ... */ style comments
    var cleanedSql = sql.replaceAll("/\\*.*?\\*/", "").trim

    // Replace -- style comments
    cleanedSql = cleanedSql.replaceAll("--.*", "")

    cleanedSql
  }

  private def removeDynamicVariables(sql: String): String = {
    // Replace :variable style dynamic variables
    val variablePattern = new Regex(":[a-zA-Z0-9_]+")
    variablePattern.replaceAllIn(sql, "")
  }
}

object SQLParser {

  val logger: Logger = LoggerFactory.getLogger(getClass)

  val NUMBER_ONE      = 1
  val NUMBER_MINUSONE = -1

  @deprecated
  private def createSQLNodesForSelect(selectStmts: List[Select]): Option[List[SQLQuery]] = {
    Some(selectStmts.flatMap {
      case plainSelect: PlainSelect if plainSelect.getFromItem.isInstanceOf[Table] =>
        val sqlTable = createSQLTableItem(plainSelect.getFromItem.asInstanceOf[Table])
        List(SQLQuery(SQLQueryType.SELECT, sqlTable, getColumns(plainSelect, sqlTable)))
      case parenthesedSelect: ParenthesedSelect =>
        createSQLNodesForSelect(List(parenthesedSelect.getPlainSelect)).getOrElse(List.empty[SQLQuery])
      /*
         Example of SetOperation SQL Queries:
        -- SELECT column_name FROM table1
        -- UNION|INTERSECT
        -- SELECT column_name FROM table2;
       */
      case setOpList: SetOperationList =>
        val selectStmts = setOpList.getSelects.asScala.toList
        val tableNameColumnListMap = selectStmts.map { stmt =>
          val plainSelect = stmt.asInstanceOf[PlainSelect]
          val sqlTable    = createSQLTableItem(plainSelect.getFromItem.asInstanceOf[Table])
          (sqlTable, getColumns(plainSelect, sqlTable))
        }

        // Merge all column lists into a single list of unique columns
        tableNameColumnListMap.map((i) => {
          SQLQuery(SQLQueryType.SELECT, i._1, i._2)
        })
    })
  }

  def parseSqlQuery(sqlQuery: String): Option[List[SQLQuery]] = {
    try {
      val cleanedSql           = SqlCleaner.clean(sqlQuery)
      val statement: Statement = CCJSqlParserUtil.parse(new StringReader(cleanedSql))
      statement match {
        case selectStmt: Select => {
          Some(
            Try(
              selectStmt.getWithItemsList.asScala
                .map(p => p.getSelect)
            ).toOption.getOrElse(List.empty[PlainSelect]).toList ++ List(selectStmt.getSelectBody)
          ).flatMap(p => createSQLNodesForSelect(p))
        }
        case insertStmt: Insert =>
          val sqlTable = createSQLTableItem(insertStmt.getTable)
          Some(
            List(
              SQLQuery(
                SQLQueryType.INSERT,
                sqlTable,
                insertStmt.getColumns.asScala.map(x => createSQLColumnItem(x, sqlTable)).toList
              )
            )
          )
        case updateStmt: Update =>
          val sqlTable = createSQLTableItem(updateStmt.getTable)
          Some(
            List(
              SQLQuery(
                SQLQueryType.UPDATE,
                sqlTable,
                updateStmt.getColumns.asScala.map(x => createSQLColumnItem(x, sqlTable)).toList
              )
            )
          )
        case createStmt: CreateTable =>
          val columns: List[String] =
            createStmt.getColumnDefinitions.asScala.map(_.getColumnName).toList

          val sqlTable = createSQLTableItem(createStmt.getTable)
          val columnList = columns.map(columnName => {
            val lineColumn = getLineAndColumnNumber(sqlQuery, columnName)
            SQLColumn(columnName, lineColumn._1, lineColumn._2)
          })
          Some(List(SQLQuery(SQLQueryType.CREATE, sqlTable, columnList)))
        case _ =>
          logger.debug("Something wrong: ", sqlQuery)
          logger.error("Something wrong: ", sqlQuery)
          None
      }
    } catch {
      case e: JSQLParserException =>
        logger.debug(s"Failed to parse the SQL query '$sqlQuery'. Error: ${e.getMessage}")
        logger.error(s"Failed to parse the SQL query '$sqlQuery'. Error: ${e.getMessage}")
        None
      case e: Exception =>
        logger.debug(s"Failed to parse the SQL query '$sqlQuery'. Error: ${e.getMessage}")
        logger.error(s"Failed to parse the SQL query '$sqlQuery'. Error: ${e.getMessage}")
        None
    }
  }

  def getColumns(plainSelect: PlainSelect, sqlTable: SQLTable): List[SQLColumn] = {
    plainSelect.getSelectItems.asScala.flatMap { (item: SelectItem[?]) =>
      item.toString match {
        case f: String if f.contains("(") =>
          val parsedResult = CCJSqlParserUtil.parseExpression(f)
          parsedResult match
            case function: Function =>
              function.getParameters.getExpressions.asScala.map(column => {
                createSQLColumnItem(column, sqlTable)
              })
            case castExpression: CastExpression =>
              List(createSQLColumnItem(castExpression.getLeftExpression, sqlTable))
            case binaryExpr: BinaryExpression => List(createSQLColumnItem(binaryExpr.getLeftExpression, sqlTable))
            case _                            => List(createSQLColumnItem(item, sqlTable))
        case _ =>
          List(createSQLColumnItem(item, sqlTable))
      }
    }.toList
  }

  private def createSQLTableItem(table: Table): SQLTable = {
    val tableName: String = table.getName
    val tableLineNumber   = Try(table.getASTNode.jjtGetFirstToken().beginLine).getOrElse(NUMBER_ONE)
    val tableColumnNumber = Try(table.getASTNode.jjtGetFirstToken().beginColumn).getOrElse(NUMBER_MINUSONE)
    SQLTable(tableName, tableLineNumber, tableColumnNumber)
  }

  private def createSQLColumnItem(column: ASTNodeAccess, sqlTable: SQLTable) = {
    SQLColumn(
      column.toString,
      Try(column.getASTNode.jjtGetFirstToken().beginLine).getOrElse(NUMBER_ONE),
      Try(column.getASTNode.jjtGetFirstToken().beginColumn).getOrElse(NUMBER_MINUSONE)
    )
  }

  private def getLineAndColumnNumber(sqlQuery: String, columnName: String) = {
    var foundLineNumber   = NUMBER_ONE
    var foundColumnNumber = NUMBER_MINUSONE
    breakable {
      sqlQuery.split("\n").zipWithIndex.foreach { case (queryLine, lineNumber) =>
        val columnNumber = queryLine.indexOf(columnName)
        if (columnNumber != NUMBER_MINUSONE) {
          foundLineNumber = lineNumber + NUMBER_ONE
          foundColumnNumber = columnNumber
          break()
        }
      }
    }
    (foundLineNumber, foundColumnNumber)
  }

}

object SQLNodeBuilder {

  def buildAndReturnIndividualQueryNode(
    builder: DiffGraphBuilder,
    fileNode: NodeOrDetachedNode,
    queryModel: SQLQuery,
    query: String,
    queryLineNumber: Int,
    queryOrder: Int,
    fileName: Option[String] = None
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
      /* As queries from .sql files are processed individually,
        an offset equal to the lineNumber of the query in the original file - 1 is added to the isolated column lineNumber */
      val lineNumber = fileName match
        case Some(f) if f.endsWith(".sql")                       => queryColumn.lineNumber + queryLineNumber - 1
        case Some(f) if f.endsWith(".yaml") | f.endsWith(".yml") => queryColumn.lineNumber + queryLineNumber
        case _                                                   => queryColumn.lineNumber

      val columnNode = NewSqlColumnNode()
        .name(queryColumn.name)
        .code(queryColumn.name)
        .lineNumber(lineNumber)
        .columnNumber(queryColumn.columnNumber)
        .order(columnIndex)
      builder.addEdge(tableNode, columnNode, EdgeTypes.AST)
      builder.addEdge(columnNode, fileNode, EdgeTypes.SOURCE_FILE)
    }
  }

  def parseQueryAndCreateNodes(
    builder: DiffGraphBuilder,
    query: String,
    fileNode: NodeOrDetachedNode,
    queryLineNumber: Int = -1,
    fileName: Option[String] = None
  ): Unit = {
    try {
      parseSqlQuery(query) match {
        case Some(parsedQueryList) =>
          parsedQueryList.zipWithIndex.foreach { case (parsedQueryItem: SQLQuery, queryOrder) =>
            SQLNodeBuilder.buildAndReturnIndividualQueryNode(
              builder,
              fileNode,
              parsedQueryItem,
              query,
              queryLineNumber,
              queryOrder,
              fileName = fileName
            )
          }
        case None =>
          logger.debug("Failed to parse query ", query)
          logger.error("Failed to parse query ", query)
      }
    } catch {
      case ex: Exception =>
        logger.error(s"Error while parsing SQL query: ${query}")
        None
    }
  }

}
