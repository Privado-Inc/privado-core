package ai.privado.tagger

import ai.privado.model.{NodeType, RuleInfo}
import ai.privado.tagger.collection.CollectionTagger
import ai.privado.tagger.sink.{APITagger, DatabaseTagger, LeakageTagger, SDKTagger}
import ai.privado.tagger.source.{IdentifierTagger, LiteralTagger}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) {

  def runTagger(rules: List[RuleInfo]): Traversal[Tag] = {

    val sourceRules = rules.filter(rule => rule.nodeType.equals(NodeType.SOURCE))
    for (rule <- sourceRules) {
      new LiteralTagger(cpg, rule).createAndApply()
      new IdentifierTagger(cpg, rule).createAndApply()
    }

    println("Now will start tagging")

    getRulesByNodeType(rules, NodeType.DATABASE).foreach(rule => new DatabaseTagger(cpg, rule).createAndApply())
    getRulesByNodeType(rules, NodeType.LEAKAGE).foreach(rule => new LeakageTagger(cpg, rule).createAndApply())
    getRulesByNodeType(rules, NodeType.SDK).foreach(rule => new SDKTagger(cpg, rule).createAndApply())
    getRulesByNodeType(rules, NodeType.API).foreach(rule => new APITagger(cpg, rule).createAndApply())

    getRulesByNodeType(rules, NodeType.COLLECTIONS).foreach(rule =>
      new CollectionTagger(cpg, rule, sourceRules).createAndApply()
    )

    cpg.tag
  }

}
