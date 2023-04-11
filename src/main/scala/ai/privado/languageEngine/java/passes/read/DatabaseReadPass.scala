package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{DataFlowCache, RuleCache, TaggerCache}
import ai.privado.dataflow.DuplicateFlowProcessor
import ai.privado.model.{Constants, DataFlowPathModel, RuleInfo}
import ai.privado.utility.SQLParser
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

//    TODO check if flow logic can be used to identify query to execute nodes
    val sqlQueryStatementStringNodes = cpg.identifier.typeFullName("java.lang.String").code("(sql|query).*").l
    val sqlQueryLocalRefNode         = sqlQueryStatementStringNodes.refsTo.dedup.l
    sqlQueryLocalRefNode.map(node => getDBReadForNode(node, builder))
  }

  def extractSQLForConcatenatedString(sqlQuery: String): String = {
    val query = sqlQuery
      .split("\\\"\\s*\\+\\s*\\\"")
      .map(_.stripMargin)
      .mkString("")

    val pattern = "(?i)SELECT\\s(.*?)\\sFROM\\s(.*?)(`.*?`|\".*?\"|'.*?'|\\w+)".r
    pattern.findFirstIn(query).getOrElse("")
  }

  def getDBReadForNode(node: Declaration, builder: DiffGraphBuilder) = {
    val sqlQuery = findSQLQueryFromReferenceNode(node)

    if (sqlQuery.nonEmpty) {
      val query  = extractSQLForConcatenatedString(sqlQuery.head)
      val result = SQLParser.parseSQL(query)

      val tableName = s"(?i).*${result._1}.*".r
      val columns   = result._2

      sensitiveClasses.find(s => s.matches(tableName.regex)) match {
        case Some(value) =>
          val ruleIds = sensitiveClassesWithMatchedRules(value).keys
          if (columns.length == 1 && columns(0) == "*") {
            ruleIds.map(ruleId => addReadFlowToExport("*", ruleId, node, builder))
          } else {
            ruleIds.map(ruleId => {
              matchColumnNameWithRules(ruleId, columns) match {
                case Some(column) => addReadFlowToExport(column, ruleId, node, builder)
                case _            => ()
              }
            })
          }
        case None => ()
      }
    }
  }

  def findSQLQueryFromReferenceNode(node: Declaration) = {
    node match {
      case local: Local =>
        findQueryFromLocalNode(local)
      case methodParamIn: MethodParameterIn =>
        methodParamIn.astParent.ast.isCall
          .name(Operators.assignment)
          .argument
          .where(_.argumentIndex(2))
          .code
          .filter(query => query.matches("(?i)^\"select.*"))
      //          || query.startsWith("\"delete")
      case _ =>
        logger.warn("Unable to match the Declaration node to Local or MethodParamIn Type", node.toString)
        val traversal: Traversal[String] = Traversal.empty[String]
        traversal
    }

  }

  def matchColumnNameWithRules(ruleId: String, columns: Array[String]): Option[String] = {
    val pattern               = RuleCache.getRuleInfo(ruleId).get.combinedRulePattern.r
    var matchedColumn: String = None.toString
    breakable {
      columns.foreach(column => {
        if (pattern.matches(column)) {
          matchedColumn = column
          break()
        }
      })
    }
    if (matchedColumn.nonEmpty) {
      Some(matchedColumn)
    } else {
      None
    }
  }

//  TODO add PrepareStatement or Execute node for flow if possible
  def addReadFlowToExport(column: String, ruleId: String, node: Declaration, builder: DiffGraphBuilder) = {
    val elements = List(node).asInstanceOf[List[AstNode]]
    val path     = Path(elements)
    val pathId   = DuplicateFlowProcessor.calculatePathId(path)
    DataFlowCache.dataflowsMapByType = DataFlowCache.dataflowsMapByType.updated(pathId.get, path)
    val dataflowModel =
      DataFlowPathModel(ruleId, "Storages.SpringFramework.Jdbc.Read", "storages", "REGULAR", pathId.get)
    DataFlowCache.setDataflow(dataflowModel)
  }

  def findQueryFromLocalNode(localNode: Local) = {
    localNode.astParent.ast.isCall
      .name(Operators.assignment)
      .where(_.lineNumber(localNode.lineNumber.get))
      .argument
      .where(_.argumentIndex(2))
      .code
      .filter(query => query.matches("(?i)^\"select.*"))
  }

}
