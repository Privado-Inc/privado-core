package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.RuleCache
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.*

protected trait TaggerHelper {

  val flinkSinkName = "addSink"

  /** Helper function to copy tags from nodeFrom to nodeTo
    *
    * @param builder
    * @param ruleCache
    * @param nodeFrom
    * @param nodeTo
    */
  def copyTags(builder: DiffGraphBuilder, ruleCache: RuleCache, nodeFrom: List[AstNode], nodeTo: AstNode): Unit =
    nodeFrom.tag.foreach(t => storeForTag(builder, nodeTo, ruleCache)(t.name, t.value))

}
