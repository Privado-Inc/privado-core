package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{DataFlowCache, RuleCache, TaggerCache}
import ai.privado.dataflow.{Dataflow, DuplicateFlowProcessor}
import ai.privado.entrypoint.ScanProcessor
import ai.privado.languageEngine.java.semantic.SemanticGenerator
import ai.privado.model.{Constants, DataFlowPathModel, InternalTag, RuleInfo}
import ai.privado.utility.SQLParser
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode}
import org.slf4j.{Logger, LoggerFactory}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}

import scala.util.control.Breaks._

class DatabaseReadPass(cpg: Cpg, taggerCache: TaggerCache) extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
  val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys

  val logger: Logger = LoggerFactory.getLogger(getClass)

//  val preparedStatementCalls =
//    cpg.call.methodFullName("java.sql.Connection.prepareStatement.*").asInstanceOf[Traversal[CfgNode]].l
//
//  val preparedStatementIdentifiers =
//    cpg.identifier.typeFullName("(java.sql.Connection.prepareStatement|java.sql.PreparedStatement).*").l

  override def generateParts(): Array[_ <: AnyRef] = {
    val readSinks = RuleCache.getAllRuleInfo
      .filter(rule => rule.catLevelTwo.equals(Constants.storages))
      .filter(rule => rule.id.matches(".*Read(?:AndWrite)?$"))
    Array(readSinks.head)
  }

  override def runOnPart(builder: DiffGraphBuilder, rule: RuleInfo): Unit = {
    val sqlQueryStatementNodes = cpg.literal
      .code(".*SELECT.*")
      .repeat(_.astParent)(_.until(_.isCall.whereNot(_.name(Operators.addition))))
      .isCall
      .argument
      .code(".*SELECT.*")
      .l
    sqlQueryStatementNodes.map(node => getDBReadForNode(node, builder))
  }

  def extractSQLForConcatenatedString(sqlQuery: String): String = {
    val query = sqlQuery
      .split("\\\"\\s*\\+\\s*\\\"")
      .map(_.stripMargin)
      .mkString("")

    val pattern = "(?i)SELECT\\s(.*?)\\sFROM\\s(.*?)(`.*?`|\".*?\"|'.*?'|\\w+)".r
    pattern.findFirstIn(query).getOrElse("")
  }

  def getDBReadForNode(node: Expression, builder: DiffGraphBuilder) = {
    val query  = extractSQLForConcatenatedString(node.code)
    val result = SQLParser.parseSQL(query)

    // Match classes which end with tableName
    val tableName = s"(?i).*${result._1}".r
    val columns   = result._2

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
            .where(_.file.nameExact(node.file.name.headOption.getOrElse("")))
            .map(_.asInstanceOf[CfgNode])
            .l
        implicit val engineContext: EngineContext =
          EngineContext(semantics = SemanticGenerator.getSemantics(cpg, ScanProcessor.config), config = EngineConfig(4))
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
        sensitiveMemberRuleIds.foreach(ruleId =>
          matchColumnNameWithRules(ruleId, columns) match {
            case Some(column) => addTagsToNode(ruleId, node, builder)
            case _            => ()
          }
        )
      else
        RuleCache.getRule.sources.foreach(rule => {
          matchColumnNameWithRules(rule.id, columns) match {
            case Some(column) => addTagsToNode(rule.id, node, builder)
            case _            => ()
          }
        })
    }
  }

//  def findSQLQueryFromReferenceNode(node: Declaration) = {
//    node match {
//      case local: Local =>
//        findQueryFromLocalNode(local)
//      case methodParamIn: MethodParameterIn =>
//        methodParamIn.astParent.ast.isCall
//          .name(Operators.assignment)
//          .argument
//          .where(_.argumentIndex(2))
//          .code
//          .filter(query => query.matches("(?i)^\"select.*"))
//      //          || query.startsWith("\"delete")
//      case _ =>
//        logger.warn("Unable to match the Declaration node to Local or MethodParamIn Type", node.toString)
//        val traversal: Traversal[String] = Traversal.empty[String]
//        traversal
//    }
//
//  }

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

//  def findQueryFromLocalNode(localNode: Local) = {
//    localNode.astParent.ast.isCall
//      .name(Operators.assignment)
//      .where(_.lineNumber(localNode.lineNumber.get))
//      .argument
//      .where(_.argumentIndex(2))
//      .code
//      .filter(query => query.matches("(?i)^\"select.*"))
//  }

}
