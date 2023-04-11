package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.InternalTag
import ai.privado.utility.SQLParser
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.util.control.Breaks._

class DatabaseReadPass(cpg: Cpg, taggerCache: TaggerCache) extends ForkJoinParallelCpgPass[Expression](cpg) {
  val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
  val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys
  val selectRegexPattern               = "(?i).*select.*"

  val logger: Logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
//    CPG query to fetch the Literal with SQL string
//    'Repeat until' is used to combine multiline SQL queries into one
    cpg.literal
      .code(selectRegexPattern)
      .repeat(_.astParent)(_.until(_.isCall.whereNot(_.name(Operators.addition))))
      .isCall
      .argument
      .code(selectRegexPattern)
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, node: Expression): Unit = {
    processDBReadNode(builder, node)
  }

  def processDBReadNode(builder: DiffGraphBuilder, node: Expression) = {
    val query  = extractSQLForConcatenatedString(node.code)
    val result = SQLParser.parseSQL(query)

    val tableName = s"(?i).*${result._1}.*".r
    val columns   = result._2

    sensitiveClasses.find(s => s.matches(tableName.regex)) match {
      case Some(value) =>
        val ruleIds = sensitiveClassesWithMatchedRules(value).keys
        if (columns.length == 1 && columns(0) == "*") {
          ruleIds.map(ruleId => addTagsToNode(ruleId, node, builder))
        } else {
          ruleIds.map(ruleId => {
            matchColumnNameWithRules(ruleId, columns) match {
              case Some(_) => addTagsToNode(ruleId, node, builder)
              case _       => ()
            }
          })
        }
      case None => ()
    }
  }

  def matchColumnNameWithRules(ruleId: String, columns: Array[String]): Option[String] = {
    val pattern                       = RuleCache.getRuleInfo(ruleId).get.combinedRulePattern.r
    var matchedColumn: Option[String] = None
    breakable {
      columns.foreach(column => {
        if (pattern.matches(column)) {
          matchedColumn = Some(column)
          break()
        }
      })
    }
    matchedColumn
  }

  def addTagsToNode(ruleId: String, node: Expression, builder: DiffGraphBuilder) = {
    storeForTag(builder, node)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
    addRuleTags(builder, node, RuleCache.getRuleInfo(ruleId).get)
  }
  def extractSQLForConcatenatedString(sqlQuery: String): String = {
    val query = sqlQuery
      .split("\\\"\\s*\\+\\s*\\\"") // Splitting the query on '+' operator and joining back to form complete query
      .map(_.stripMargin)
      .mkString("")

    val pattern =
      "(?i)SELECT\\s(.*?)\\sFROM\\s(.*?)(`.*?`|\".*?\"|'.*?'|\\w+)".r // Pattern to fetch the SELECT statement from the query
    pattern.findFirstIn(query).getOrElse("")
  }

}
