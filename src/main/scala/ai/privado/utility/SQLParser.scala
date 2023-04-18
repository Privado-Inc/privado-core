package ai.privado.utility

import java.io.StringReader
import scala.util.matching.Regex
import net.sf.jsqlparser.JSQLParserException
import net.sf.jsqlparser.expression.Function
import net.sf.jsqlparser.parser.CCJSqlParserUtil
import net.sf.jsqlparser.statement.Statement
import net.sf.jsqlparser.statement.create.table.{ColumnDefinition, CreateTable}
import net.sf.jsqlparser.statement.delete.Delete
import net.sf.jsqlparser.statement.drop.Drop
import net.sf.jsqlparser.statement.insert.Insert
import net.sf.jsqlparser.statement.select.{PlainSelect, Select, SelectItem, SetOperationList}
import net.sf.jsqlparser.statement.update.Update
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

object SqlCleaner {
  def clean(sql: String): String = {
    var cleanedSql = removeComments(sql)
    cleanedSql = removeDynamicVariables(cleanedSql)
    cleanedSql
  }

  private def removeComments(sql: String): String = {
    // Replace /* ... */ style comments
    var cleanedSql = sql.replaceAll("/\\*.*?\\*/", "")

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

  def getColumns(plainSelect: PlainSelect): List[String] = {
    plainSelect.getSelectItems.flatMap { case item: SelectItem =>
      item.toString match {
        case f: String if f.contains("(") =>
          val function = CCJSqlParserUtil.parseExpression(f).asInstanceOf[Function]
          function.getParameters.getExpressions.map(_.toString)
        case _ => List(item.toString)
      }
    }.toList
  }

  def parseSqlQuery(sqlQuery: String): Option[List[(String, String, List[String])]] = {
    try {
      val cleanedSql           = SqlCleaner.clean(sqlQuery)
      val statement: Statement = CCJSqlParserUtil.parse(new StringReader(cleanedSql))
      statement match {
        case selectStmt: Select =>
          selectStmt.getSelectBody match {
            case plainSelect: PlainSelect =>
              val tableName: String = plainSelect.getFromItem.toString
              Some(List(("SELECT", tableName, getColumns(plainSelect))))

            /*
            Example of SetOperation SQL Queries:
              -- SELECT column_name FROM table1
              -- UNION|INTERSECT
                -- SELECT column_name FROM table2;
             */
            case setOpList: SetOperationList =>
              val selectStmts = setOpList.getSelects.toList
              val tableNameColumnListMap = selectStmts.map { stmt =>
                val plainSelect = stmt.asInstanceOf[PlainSelect]
                val tableName   = plainSelect.getFromItem.toString
                (tableName, getColumns(plainSelect))
              }

              // Merge all column lists into a single list of unique columns
              Some(tableNameColumnListMap.map((i) => {
                ("SELECT", i._1, i._2.distinct)
              }))

            case _ => None

          }
        case insertStmt: Insert =>
          val tableName: String = insertStmt.getTable.toString
          val columns: List[String] = Option(insertStmt.getColumns)
            .filterNot(_.isEmpty)
            .map(_.toArray.map(_.toString).toList)
            .getOrElse(List())
          Some(List(("INSERT", tableName, columns)))
        case updateStmt: Update =>
          val tableName: String = updateStmt.getTable.toString
          val columns: List[String] = Option(updateStmt.getColumns)
            .filterNot(_.isEmpty)
            .map(_.toArray.map(_.toString).toList)
            .getOrElse(List())
          Some(List(("UPDATE", tableName, columns)))
        case deleteStmt: Delete =>
          val tableName: String = deleteStmt.getTable.getName
          Some(List(("DELETE", tableName, List.empty[String])))
        case dropStmt: Drop =>
          val tableName = dropStmt.getName.toString
          Some(List(("DELETE", tableName, List.empty[String])))
        case createStmt: CreateTable =>
          val tableName: String = createStmt.getTable.getName
          val columns: List[String] =
            createStmt.getColumnDefinitions.toArray.map(_.asInstanceOf[ColumnDefinition].getColumnName).toList
          Some(List(("CREATE", tableName, columns)))
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

}
