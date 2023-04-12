package ai.privado.utility

import net.sf.jsqlparser.JSQLParserException
import net.sf.jsqlparser.parser.CCJSqlParserUtil
import net.sf.jsqlparser.schema.Table
import net.sf.jsqlparser.statement.select.{PlainSelect, Select}
import org.slf4j.{Logger, LoggerFactory}

object SQLParser {

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def parseSQL(sqlQuery: String) = {
    try {
      val statement = CCJSqlParserUtil.parse(sqlQuery)
      if (statement.isInstanceOf[Select]) {
        val select      = statement.asInstanceOf[Select]
        val plainSelect = select.getSelectBody.asInstanceOf[PlainSelect]
        val columns     = plainSelect.getSelectItems.toArray.map(_.toString)
        val table       = plainSelect.getFromItem.asInstanceOf[Table].toString
        (table, columns)
      } else {
        (None, Array(""))
      }
    } catch {
      case ex: JSQLParserException =>
        logger.warn("Failed to parse the SQL query ", sqlQuery)
        (None, Array(""))
    }

  }

}
