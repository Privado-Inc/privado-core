package ai.privado.utility

import net.sf.jsqlparser.JSQLParserException
import net.sf.jsqlparser.parser.CCJSqlParserUtil
import net.sf.jsqlparser.statement.Statement
import net.sf.jsqlparser.statement.create.table.{ColumnDefinition, CreateTable}
import net.sf.jsqlparser.statement.delete.Delete
import net.sf.jsqlparser.statement.insert.Insert
import net.sf.jsqlparser.statement.select.{PlainSelect, Select, SelectItem}
import net.sf.jsqlparser.statement.update.Update
import org.slf4j.{Logger, LoggerFactory}
import net.sf.jsqlparser.expression.Function

import java.io.StringReader
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`

object SQLParser {

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def parseSqlQuery(sqlQuery: String): Option[(String, String, List[String])] = {
    try {
      val statement: Statement = CCJSqlParserUtil.parse(new StringReader(sqlQuery))
      statement match {
        case selectStmt: Select =>
          val selectBody: PlainSelect = selectStmt.getSelectBody.asInstanceOf[PlainSelect]
          val tableName: String       = selectBody.getFromItem.toString
          val columns: List[String] =
            selectBody.getSelectItems.flatMap { case item: SelectItem =>
              item.toString match {
                case f: String if f.contains("(") =>
                  val function = CCJSqlParserUtil.parseExpression(f).asInstanceOf[Function]
                  function.getParameters.getExpressions.map(_.toString)
                case _ => List(item.toString)
              }
            }.toList
          Some(("SELECT", tableName, columns))
        case insertStmt: Insert =>
          val tableName: String     = insertStmt.getTable.toString
          val columns: List[String] = insertStmt.getColumns.toArray.map(_.toString).toList
          Some(("INSERT", tableName, columns))
        case updateStmt: Update =>
          val tableName: String     = updateStmt.getTable.toString
          val columns: List[String] = updateStmt.getColumns.toArray.map(_.toString).toList
          Some(("UPDATE", tableName, columns))
        case deleteStmt: Delete =>
          val tableName: String = deleteStmt.getTable.toString
          Some(("DELETE", tableName, List.empty[String]))
        case createStmt: CreateTable =>
          val tableName: String = createStmt.getTable.getName
          val columns: List[String] =
            createStmt.getColumnDefinitions.toArray.map(_.asInstanceOf[ColumnDefinition].getColumnName).toList
          Some(("CREATE", tableName, columns))
        case _ =>
          None
      }
    } catch {
      case _: JSQLParserException =>
        logger.debug("Failed to parse the SQL query ", sqlQuery)
        None
      case _: Exception =>
        logger.debug("Failed to parse the SQL query ", sqlQuery)
        None
    }
  }

}
