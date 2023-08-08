package ai.privado.utility

import ai.privado.model.sql.{SQLColumn, SQLQuery, SQLQueryType, SQLTable}

import java.io.StringReader
import scala.util.matching.Regex
import net.sf.jsqlparser.JSQLParserException
import net.sf.jsqlparser.expression.Function
import net.sf.jsqlparser.parser.{ASTNodeAccess, CCJSqlParserUtil}
import net.sf.jsqlparser.schema.Table
import net.sf.jsqlparser.statement.Statement
import net.sf.jsqlparser.statement.create.table.CreateTable
import net.sf.jsqlparser.statement.insert.Insert
import net.sf.jsqlparser.statement.select.{PlainSelect, Select, SelectItem, SetOperationList}
import net.sf.jsqlparser.statement.update.Update
import org.slf4j.{Logger, LoggerFactory}

import scala.jdk.CollectionConverters._
import scala.util.Try

import scala.util.control.Breaks.{break, breakable}

object SqlCleaner {
  def clean(sql: String): String = {
    var cleanedSql = removeComments(sql)
    cleanedSql = removeDynamicVariables(cleanedSql)
    cleanedSql
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

  def parseSqlQuery(sqlQuery: String): Option[List[SQLQuery]] = {
    try {
      val cleanedSql           = SqlCleaner.clean(sqlQuery)
      val statement: Statement = CCJSqlParserUtil.parse(new StringReader(cleanedSql))
      statement match {
        case selectStmt: Select =>
          selectStmt.getSelectBody match {
            case plainSelect: PlainSelect =>
              Some(
                List(
                  SQLQuery(
                    SQLQueryType.SELECT,
                    createSQLTableItem(plainSelect.getFromItem.asInstanceOf[Table]),
                    getColumns(plainSelect)
                  )
                )
              )
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
                val table       = plainSelect.getFromItem.asInstanceOf[Table]
                (table, getColumns(plainSelect))
              }

              // Merge all column lists into a single list of unique columns
              Some(tableNameColumnListMap.map((i) => {
                SQLQuery(SQLQueryType.SELECT, createSQLTableItem(i._1), i._2)
              }))

            case _ => None

          }
        case insertStmt: Insert =>
          Some(
            List(
              SQLQuery(
                SQLQueryType.INSERT,
                createSQLTableItem(insertStmt.getTable),
                insertStmt.getColumns.asScala.map(createSQLColumnItem).toList
              )
            )
          )
        case updateStmt: Update =>
          Some(
            List(
              SQLQuery(
                SQLQueryType.UPDATE,
                createSQLTableItem(updateStmt.getTable),
                updateStmt.getColumns.asScala.map(createSQLColumnItem).toList
              )
            )
          )
        case createStmt: CreateTable =>
          val columns: List[String] =
            createStmt.getColumnDefinitions.asScala.map(colDef => cleanString(colDef.getColumnName)).toList

          val columnList = columns.map(columnName => {
            val lineColumn = getLineAndColumnNumber(sqlQuery, columnName)
            SQLColumn(columnName, lineColumn._1, lineColumn._2)
          })
          Some(List(SQLQuery(SQLQueryType.CREATE, createSQLTableItem(createStmt.getTable), columnList)))
        case _ =>
          logger.debug("Something wrong: ", sqlQuery)
          None
      }
    } catch {
      case e: JSQLParserException =>
        logger.debug(s"Failed to parse the SQL query '$sqlQuery'. Error: ${e.getMessage}")
        None
      case e: Exception =>
        logger.debug(s"Failed to parse the SQL query '$sqlQuery'. Error: ${e.getMessage}")
        None
    }
  }

  def getColumns(plainSelect: PlainSelect): List[SQLColumn] = {
    plainSelect.getSelectItems.asScala.flatMap { (item: SelectItem) =>
      item.toString match {
        case f: String if f.contains("(") =>
          val function = CCJSqlParserUtil.parseExpression(f).asInstanceOf[Function]
          function.getParameters.getExpressions.asScala.map(column => createSQLColumnItem(column))
        case _ =>
          List(createSQLColumnItem(item))
      }
    }.toList
  }

  private def createSQLTableItem(table: Table) = {
    val tableName: String = cleanString(table.getName)
    val tableLineNumber   = Try(table.getASTNode.jjtGetFirstToken().beginLine).getOrElse(NUMBER_ONE)
    val tableColumnNumber = Try(table.getASTNode.jjtGetFirstToken().beginColumn).getOrElse(NUMBER_MINUSONE)
    SQLTable(tableName, tableLineNumber, tableColumnNumber)
  }

  private def createSQLColumnItem(column: ASTNodeAccess) = {
    SQLColumn(
      cleanString(column.toString),
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

  private def cleanString(value: String) = {
    value.stripPrefix("`").stripSuffix("`")
  }

}
