package ai.privado.languageEngine.ruby.tagger.monolith

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.feeder.LeakageRule
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.{addRuleTags, resolver, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, File}
import io.shiftleft.semanticcpg.language.*

import scala.util.Try
class MonolithTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[File](cpg) {
  override def generateParts(): Array[File] = {
    cpg.file.name(".*_controller.rb").filterNot(_.name.startsWith("/")).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, fileNode: File): Unit = {

    val repoItemName = fileNode.name.replaceAll("/", "--").stripSuffix("_controller.rb")
    storeForTag(builder, fileNode, ruleCache)(Constants.monolithRepoItem, repoItemName)

    // 0th level tagging
    fileNode.ast.where(_.tag).foreach(applyMonolithTag(builder, _, repoItemName))

    var monolithDepthCounter = 1
    var startingNodes        = fileNode.ast.isCall.callee.ast

    val monolithRepoDepthFromConfig = Try(ruleCache.getSystemConfigByKey(Constants.monolithRepoDepth).toInt).toOption
    val monolithRepoDepth           = if (monolithRepoDepthFromConfig.isDefined) monolithRepoDepthFromConfig.get else 4

    while (monolithDepthCounter <= 4) {
      startingNodes.where(_.tag).foreach(applyMonolithTag(builder, _, repoItemName))

      // Reset startingNodes to the next depth
      startingNodes = startingNodes.isCall.callee.ast
      monolithDepthCounter += 1
    }

  }

  private def applyMonolithTag(builder: DiffGraphBuilder, node: AstNode, repoItemName: String) =
    storeForTag(builder, node, ruleCache)(Constants.monolithRepoItem, repoItemName)

}
