package ai.privado.languageEngine.python.passes.read

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.ScanProcessor
import ai.privado.model.InternalTag
import ai.privado.model.sql.SQLQuery
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.{SQLParser, Utilities}
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

class DatabaseReadPass(cpg: Cpg, ruleCache: RuleCache, taggerCache: TaggerCache)
    extends PrivadoParallelCpgPass[Expression](cpg) {
  val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
  val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys
  val selectRegexPattern               = "(?i).*select.*"

  val logger: Logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
//    CPG query to fetch the Literal with SQL string
//    'Repeat until' is used to combine multiline SQL queries into one
    cpg.literal
      .code(selectRegexPattern)
      .repeat(_.astParent)(_.until(_.isCall.whereNot(_.name(s"${Operators.addition}|<operator>.stringExpressionList"))))
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
        value.foreach { case queryModel: SQLQuery =>
          // Match classes which end with tableName
          val tableName = queryModel.table.name
          val columns   = queryModel.column.map(_.name)
          val sensitiveMemberRuleIds = sensitiveClasses.find(s => s.matches(s"(?i).*${tableName}")) match {
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

              val readFlow = dataElementSinks.reachableByFlows(node)(Utilities.getEngineContext(4)).l
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
              ruleCache.getRule.sources
                .filter(rule => isColumnNameMatchingWithRule(rule.id, columns))
                .foreach(rule => addTagsToNode(rule.id, node, builder))

          }
        }
      case None =>
    }

  }

  /** Return True if any column name matches the pattern
    * @param ruleId
    * @param columns
    * @return
    */
  def isColumnNameMatchingWithRule(ruleId: String, columns: List[String]): Boolean = {
    val pattern = ruleCache.getRuleInfo(ruleId).get.combinedRulePattern.r
    columns.map(pattern.matches).foldLeft(false)(_ || _)
  }

  def addTagsToNode(ruleId: String, node: Expression, builder: DiffGraphBuilder) = {
    storeForTag(builder, node, ruleCache)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
    addRuleTags(builder, node, ruleCache.getRuleInfo(ruleId).get, ruleCache)
  }
  def extractSQLForConcatenatedString(sqlQuery: String): String = {
    val query = sqlQuery
      .split(
        "\\|\\+|\\\"|\n|\\{|\\}"
      ) // Splitting the query on '+', '\"', '{' amd '}' operator and joining back to form complete query
      .map(_.stripMargin)
      .mkString("")

    val pattern =
      "(?i)SELECT\\s+(.*)\\sFROM.+".r // Pattern to fetch the SELECT statement from the query
    pattern.findFirstIn(query).getOrElse("")
  }

}
