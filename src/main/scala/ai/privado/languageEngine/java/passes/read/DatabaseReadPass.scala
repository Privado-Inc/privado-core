package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{DataFlowCache, RuleCache, TaggerCache}
import ai.privado.dataflow.DuplicateFlowProcessor
import ai.privado.model.{Constants, DataFlowPathModel, InternalTag, RuleInfo}
import ai.privado.utility.SQLParser
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal
import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode}
import io.shiftleft.semanticcpg.language._
import org.slf4j.{Logger, LoggerFactory}

import scala.util.control.Breaks._

class DatabaseReadPass(cpg: Cpg, taggerCache: TaggerCache) extends ForkJoinParallelCpgPass[Expression](cpg) {
  val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
  val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys

  val logger: Logger = LoggerFactory.getLogger(getClass)

//  val preparedStatementCalls =
//    cpg.call.methodFullName("java.sql.Connection.prepareStatement.*").asInstanceOf[Traversal[CfgNode]].l
//
//  val preparedStatementIdentifiers =
//    cpg.identifier.typeFullName("(java.sql.Connection.prepareStatement|java.sql.PreparedStatement).*").l

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.literal
      .code(".*SELECT.*")
      .repeat(_.astParent)(_.until(_.isCall.whereNot(_.name(Operators.addition))))
      .isCall
      .argument
      .code(".*SELECT.*")
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, node: Expression): Unit = {
    getDBReadForNode(builder, node)
  }

  def getDBReadForNode(builder: DiffGraphBuilder, node: Expression) = {
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
      .split("\\\"\\s*\\+\\s*\\\"")
      .map(_.stripMargin)
      .mkString("")

    val pattern = "(?i)SELECT\\s(.*?)\\sFROM\\s(.*?)(`.*?`|\".*?\"|'.*?'|\\w+)".r
    pattern.findFirstIn(query).getOrElse("")
  }

}
