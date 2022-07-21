package ai.privado.tagger.source

import ai.privado.model.{InternalTags, RuleInfo}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.{BatchedUpdate, NodeRef}

class LiteralTagger(cpg: Cpg, rule: RuleInfo) extends SimpleCpgPass(cpg){
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    //Step 1.2
    val literals = cpg.literal.code(rule.pattern).l
    literals.foreach(literal => builder.addEdge(literal,
      NewTag().name(InternalTags.VARIABLE_REGEX_LITERAL.toString), EdgeTypes.TAGGED_BY))

      literals.foreach((literal => addTags(builder, literal, rule, "SOURCE")))

  }

  def addTags(builder: DiffGraphBuilder, node: AstNode, ruleInfo: RuleInfo, nodeType: String): Unit = {
    builder.addEdge(node, NewTag().name("id").value(ruleInfo.id), EdgeTypes.TAGGED_BY)
    builder.addEdge(node, NewTag().name("name").value(ruleInfo.name), EdgeTypes.TAGGED_BY)
    builder.addEdge(node, NewTag().name("category").value(ruleInfo.category), EdgeTypes.TAGGED_BY)
    builder.addEdge(node, NewTag().name("nodeType").value(nodeType), EdgeTypes.TAGGED_BY)
    for((key, value) <- ruleInfo.tags) {
      builder.addEdge(node, NewTag().name(key).value(value), EdgeTypes.TAGGED_BY)
    }
  }
}
