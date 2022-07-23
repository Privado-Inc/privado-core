package ai.privado.utility

import ai.privado.model.{KeyConstants, RuleInfo}
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewTag
import io.shiftleft.utils.ProjectRoot
import overflowdb.{BatchedUpdate, NodeOrDetachedNode}

object Utilities {

  /*
   Utility to add a single tag to a object
   */
  def storeForTag(
    builder: BatchedUpdate.DiffGraphBuilder,
    source: NodeOrDetachedNode
  )(tagName: String, tagValue: String = "") = {
    builder.addEdge(source, NewTag().name(tagName).value(tagValue), EdgeTypes.TAGGED_BY)
  }

  /*
   Utility to add Tag based on a rule Object
   */
  def addRuleTags(builder: BatchedUpdate.DiffGraphBuilder, node: NodeOrDetachedNode, ruleInfo: RuleInfo): Unit = {
    val storeForTagHelper = storeForTag(builder, node) _
    storeForTagHelper(KeyConstants.id, ruleInfo.id)
    storeForTagHelper(KeyConstants.name, ruleInfo.name)
    storeForTagHelper(KeyConstants.category, ruleInfo.category)
    storeForTagHelper(KeyConstants.nodeType, ruleInfo.nodeType)
    for ((key, value) <- ruleInfo.tags) {
      storeForTagHelper(key, value)
    }
  }

  /*
   Utility to get the default semantics for dataflow queries
   */
  def getDefaultSemantics() = {
    val semanticsFilename = ProjectRoot.relativise("src/main/resources/default.semantics")
    println(s"Using semantics from : $semanticsFilename")
    Semantics.fromList(new Parser().parseFile(semanticsFilename))
  }

  /*
   Utility to filter rules by node type
   */
  def getRulesByNodeType(rules: List[RuleInfo], nodeType: String) = rules.filter(rule => rule.nodeType.equals(nodeType))
}
