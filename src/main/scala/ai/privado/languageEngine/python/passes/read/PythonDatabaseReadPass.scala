package ai.privado.languageEngine.python.passes.read

import ai.privado.cache.TaggerCache
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.passes.ForkJoinParallelCpgPass

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

class PythonDatabaseReadPass(cpg: Cpg, taggerCache: TaggerCache) extends ForkJoinParallelCpgPass[TypeDecl](cpg) {
  val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
  val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys
  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.typeDecl
      .where(_.inheritsFromTypeFullName.filter(inheritance => inheritance.matches("django.*Model")))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, node: TypeDecl): Unit = {
    processDBReadNode(builder, node);
  }

  private def processDBReadNode(builder: DiffGraphBuilder, decl: TypeDecl): Unit = {
    // Check if calls like <ClassName>.objects.filter/get are present

    val readCalls = cpg
      .call("(filter|get|values|values_list)")
      .where(_.methodFullName(s".*${decl.name}.*objects.*(filter|get).*(values|values_list)?.*"))
      .code
      .l

    val tableName = s".*${decl.name}".r

    val sensitiveMemberRuleIds = sensitiveClasses.find(s => s.matches(tableName.regex)) match {
      case Some(value) => sensitiveClassesWithMatchedRules(value).keys.l
      case None        => List.empty
    }

    if (readCalls.nonEmpty) {
      // Read method is present for a given class
      val columns = cpg.member.where(_.typeDecl.fullName(s".*${decl.name}.*")).map(mem => mem.name).toArray

      if (sensitiveMemberRuleIds.nonEmpty)
        sensitiveMemberRuleIds
          .filter(ruleId => isColumnNameMatchingWithRule(ruleId, columns))
          .foreach(ruleId => addTagsToNode(ruleId, decl, builder))
      else
        RuleCache.getRule.sources
          .filter(rule => isColumnNameMatchingWithRule(rule.id, columns))
          .foreach(rule => addTagsToNode(rule.id, decl, builder))

    }

  }

  def isColumnNameMatchingWithRule(ruleId: String, columns: Array[String]): Boolean = {
    val pattern = RuleCache.getRuleInfo(ruleId).get.combinedRulePattern.r
    columns.map(pattern.matches).foldLeft(false)(_ || _)
  }

  def addTagsToNode(ruleId: String, node: TypeDecl, builder: DiffGraphBuilder) = {
    storeForTag(builder, node)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
    addRuleTags(builder, node, RuleCache.getRuleInfo(ruleId).get)
  }

}
