package ai.privado.languageEngine.ruby.tagger.monolith

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.feeder.LeakageRule
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, File}
import io.shiftleft.semanticcpg.language.*
class MonolithTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[File](cpg) {
  override def generateParts(): Array[File] = {
    cpg.file.name(".*_controller.rb").toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, fileNode: File): Unit = {

    fileNode.ast.where(_.tag).foreach(storeForTag(builder, _, ruleCache)(Constants.monolithRepoItem, fileNode.name))
  }

}
