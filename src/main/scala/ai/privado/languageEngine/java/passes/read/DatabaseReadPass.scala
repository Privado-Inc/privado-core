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

class DatabaseReadPass(cpg: Cpg, taggerCache: TaggerCache, classTableMapping: Map[String, TypeDecl])
    extends ForkJoinParallelCpgPass[Expression](cpg) {
  val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
  val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys.l
  val selectRegexPattern               = "(?i)(\")?\\s{0,5}select\\s+.*"
  val fromRegexPattern                 = "(?i)(\")?\\s{0,5}from\\s+.*"

  val logger: Logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
//    CPG query to fetch the Literal with SQL string
//    'Repeat until' is used to combine multiline SQL queries into one
    cpg.literal
      .or(_.code(selectRegexPattern), _.code(fromRegexPattern))
      .repeat(_.astParent)(_.until(_.isCall.whereNot(_.name(Operators.addition))))
      .isCall
      .argument
      .or(_.code(selectRegexPattern), _.code(fromRegexPattern))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, node: Expression): Unit = {
    processDBReadNode(builder, node)
  }

  def processDBReadNode(builder: DiffGraphBuilder, node: Expression) = {
    val query  = extractSQLForConcatenatedString(node.code)
    val result = SQLParser.parseSQL(query)

    result match {
      case Some(value) =>
        // Match classes which end with tableNameRegex
        val tableName      = value._1.toLowerCase
        val tableNameRegex = s"(?i).*$tableName".r
        val columns        = value._2

        val sensitiveMemberRuleIds = {
          if (classTableMapping.contains(tableName) && sensitiveClasses.contains(classTableMapping(tableName).fullName))
            sensitiveClassesWithMatchedRules(classTableMapping(tableName).fullName).keys.l
          else
            sensitiveClasses.find(s => s.matches(tableNameRegex.regex)) match {
              case Some(value) => sensitiveClassesWithMatchedRules(value).keys.l
              case None        => List.empty
            }
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
  def isColumnNameMatchingWithRule(ruleId: String, columns: Array[String]): Boolean = {
    val pattern = RuleCache.getRuleInfo(ruleId).get.combinedRulePattern.r
    columns.map(pattern.matches).foldLeft(false)(_ || _)
  }

  def addTagsToNode(ruleId: String, node: Expression, builder: DiffGraphBuilder) = {
    storeForTag(builder, node)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
    addRuleTags(builder, node, RuleCache.getRuleInfo(ruleId).get)
  }
  def extractSQLForConcatenatedString(sqlQuery: String): String = {
    var query = sqlQuery
      .stripPrefix("\"")
      .stripSuffix("\"")
      .split("\\\"\\s*\\+\\s*\\\"") // Splitting the query on '+' operator and joining back to form complete query
      .map(_.stripMargin)
      .mkString("")

    // Add `select *` to queries which are `from users`
    if (query.matches(fromRegexPattern))
      query = "select * " + query

    val pattern =
      "(?i)SELECT\\s(.*?)\\sFROM\\s(.*?)(`.*?`|\".*?\"|'.*?'|\\w+)".r // Pattern to fetch the SELECT statement from the query
    pattern.findFirstIn(query).getOrElse("")
  }

}
