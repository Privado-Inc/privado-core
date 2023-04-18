package ai.privado.utility

import java.io.StringReader
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

import scala.jdk.CollectionConverters
import scala.collection.JavaConverters.iterableAsScalaIterableConverter

object SQLParser {

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def parseSqlQuery(sqlQuery: String): Option[List[(String, String, List[String])]] = {
    try {
      val statement: Statement = CCJSqlParserUtil.parse(new StringReader(sqlQuery))
      statement match {
        case selectStmt: Select =>
          selectStmt.getSelectBody match {
            case plainSelect: PlainSelect =>
              val tableName: String = plainSelect.getFromItem.toString
              val columns: List[String] =
                plainSelect.getSelectItems.asScala.flatMap { case item: SelectItem =>
                  item.toString match {
                    case f: String if f.contains("(") =>
                      val function = CCJSqlParserUtil.parseExpression(f).asInstanceOf[Function]
                      function.getParameters.getExpressions.toString.map(_.toString)
                    case _ => List(item.toString)
                  }
                }.toList
              Some(List(("SELECT", tableName, columns)))

            case setOpList: SetOperationList =>
              val selectStmts = setOpList.getSelects.asScala.toList
              val tableNameColumnListMap = selectStmts.map { stmt =>
                val plainSelect = stmt.asInstanceOf[PlainSelect]
                val tableName   = plainSelect.getFromItem.toString
                (
                  tableName,
                  plainSelect.getSelectItems.asScala.flatMap { case item: SelectItem =>
                    item.toString match {
                      case f: String if f.contains("(") =>
                        val function = CCJSqlParserUtil.parseExpression(f).asInstanceOf[Function]
                        function.getParameters.getExpressions.toString.map(_.toString)
                      case _ => List(item.toString)
                    }
                  }.toList
                )
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
          println("Something wrong: ", sqlQuery)
          None
      }
    } catch {
      case e: JSQLParserException =>
        println(s"Failed to parse the SQL query '$sqlQuery'. Error: ${e.getMessage}")
        None
      case e: Exception =>
        println(s"Failed to parse the SQL query '$sqlQuery'. Error: ${e.getMessage}")
        None
    }
  }

}
