package ai.privado.utility

import net.sf.jsqlparser.parser.CCJSqlParserUtil
import net.sf.jsqlparser.schema.Table
import net.sf.jsqlparser.statement.select.{PlainSelect, Select}

object SQLParser {

  def parseSQL(sqlQuery: String) = {
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
  }

}
