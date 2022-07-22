package ai.privado.utility

import ai.privado.model.RuleInfo
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewTag
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}

object Utilities {

  /*
   Utility to add a single tag to a object
   */
  def storeForTag(builder: BatchedUpdate.DiffGraphBuilder, source : NodeOrDetachedNode)( tagName: String, tagValue: String = "") = {
    builder.addEdge(source, NewTag().name(tagName).value(tagValue), EdgeTypes.TAGGED_BY)
  }

  /*
   Utility to add Tag based on a rule Object
   */
  def addRuleTags(builder: BatchedUpdate.DiffGraphBuilder, node: NodeOrDetachedNode, ruleInfo: RuleInfo, nodeType: String): Unit = {
    val storeForTagHelper = storeForTag(builder, node)_
    storeForTagHelper("id", ruleInfo.id)
    storeForTagHelper("name", ruleInfo.name)
    storeForTagHelper("category", ruleInfo.category)
    storeForTagHelper("nodeType", nodeType)
    for((key, value) <- ruleInfo.tags) {
      storeForTagHelper(key, value)
    }
  }
}
