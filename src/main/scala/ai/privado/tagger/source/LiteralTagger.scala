package ai.privado.tagger.source

import ai.privado.model.{InternalTags, NodeType, RuleInfo}
import io.shiftleft.codepropertygraph.generated.nodes.{NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.{BatchedUpdate}
import ai.privado.utility.Utilities._

class LiteralTagger(cpg: Cpg, rule: RuleInfo) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    // Step 1.2
    val literals = cpg.literal.code(rule.patterns.head).l
    literals.foreach(literal =>
      builder.addEdge(literal, NewTag().name(InternalTags.VARIABLE_REGEX_LITERAL.toString), EdgeTypes.TAGGED_BY)
    )

    literals.foreach((literal => addRuleTags(builder, literal, rule)))

  }

}
