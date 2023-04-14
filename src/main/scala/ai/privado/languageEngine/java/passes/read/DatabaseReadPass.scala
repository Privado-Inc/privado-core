package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.model.InternalTag
import ai.privado.utility.SQLParser
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import org.slf4j.{Logger, LoggerFactory}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.language._
import net.sf.jsqlparser.parser.{CCJSqlParser, CCJSqlParserUtil}
import net.sf.jsqlparser.statement.Statement

import java.io.{FileReader, InputStreamReader, StringReader}
import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile

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
    val result = SQLParser.parseSqlQuery(query)

    result match {
      case Some(value) =>
        // Match classes which end with tableName
        val tableName = s"(?i).*${value._2}".r
        val columns   = value._3

        val sensitiveMemberRuleIds = sensitiveClasses.find(s => s.matches(tableName.regex)) match {
          case Some(value) => sensitiveClassesWithMatchedRules(value).keys.l
          case None        => List.empty
        }

        if (columns.length == 1 && columns(0) == "*") {
          if (sensitiveMemberRuleIds.nonEmpty)
            sensitiveMemberRuleIds.foreach(ruleId => addTagsToNode(ruleId, node, builder))
          else {
            /* Run dataflow and verify the data-elements read from the call,
                Ex - resultSet = statement.executeQuery("SELECT * FROM mytable");
                // Loop through the result set and print out each row
                while (resultSet.next()) {
                    int id = resultSet.getInt("id");
                    String firstName = resultSet.getString("name");
                    int age = resultSet.getInt("age");
                    System.out.println("ID: " + id + ", Name: " + firstName + ", Age: " + age)
                }
             */
            val dataElementSinks =
              Dataflow
                .getSources(cpg)
                .filterNot(_.isMember)
                .map(_.asInstanceOf[CfgNode])
                .l
            implicit val engineContext: EngineContext =
              EngineContext(config = EngineConfig(4))
            val readFlow = dataElementSinks.reachableByFlows(node).l
            if (readFlow.nonEmpty) {
              // As a flow is present from Select query to a Data element we can say, the data element is read from the query
              readFlow
                .flatMap(_.elements.last.tag.value("Data.Sensitive.*"))
                .value
                .foreach(ruleId => addTagsToNode(ruleId, node, builder))
            }
          }
        } else {
          if (sensitiveMemberRuleIds.nonEmpty)
            sensitiveMemberRuleIds
              .filter(ruleId => isColumnNameMatchingWithRule(ruleId, columns))
              .foreach(ruleId => addTagsToNode(ruleId, node, builder))
          else
            RuleCache.getRule.sources
              .filter(rule => isColumnNameMatchingWithRule(rule.id, columns))
              .foreach(rule => addTagsToNode(rule.id, node, builder))
        }
      case None => ()
    }

  }

  /** Return True if any column name matches the pattern
    * @param ruleId
    * @param columns
    * @return
    */
  def isColumnNameMatchingWithRule(ruleId: String, columns: List[String]): Boolean = {
    val pattern = RuleCache.getRuleInfo(ruleId).get.combinedRulePattern.r
    columns.map(pattern.matches).foldLeft(false)(_ || _)
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

/*
object sqlTest extends App {

  import org.jooq.SQLDialect
  import org.jooq.impl.DSL
  import scala.jdk.CollectionConverters._

  val sqlFilePath      = "/Users/khemrajrathore/Privado/joern-testing/ISIDrone/ISIDrone/sample.sql"
  val sqlFile          = fromFile(sqlFilePath)
  val parsedStatements = DSL.using(SQLDialect.DEFAULT).parser.parse(sqlFile.mkString).asScala.toList
  val queries          = parsedStatements.collect { case q: org.jooq.Query => q }
  val sqlQueries       = queries.map(_.getSQL())

  println(sqlQueries)
  sqlQueries.foreach(query => {
    println(query)
    SQLParser.parseSqlQuery(query) match {
      case Some((queryName, tableName, columns)) => println(queryName, tableName, columns.mkString(" "))
      case None                                  =>
    }
  })
  sqlFile.close()
}

 */
