package ai.privado.tagger

import ai.privado.model.{NodeType, RuleInfo}
import ai.privado.tagger.sink.{APITagger, DatabaseTagger, LeakageTagger, SDKTagger}
import ai.privado.tagger.source.{IdentifierTagger, LiteralTagger}
import ai.privado.utility.Utilities._
import io.joern.joerncli.CpgBasedTool.addDataFlowOverlayIfNonExistent
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class PrivadoTagger(cpg: Cpg) {

  def runTagger(rules: List[RuleInfo]): Traversal[Tag] = {

    val sourceRules = rules.filter(rule => rule.nodeType.equals(NodeType.SOURCE.toString))
    for (rule <- sourceRules) {
      new LiteralTagger(cpg, rule).createAndApply()
      new IdentifierTagger(cpg, rule).createAndApply()
    }
    addDataFlowOverlayIfNonExistent(cpg)

    println("Now will start tagging")

    getRulesByNodeType(rules, NodeType.DATABASE.toString).foreach(rule =>
      new DatabaseTagger(cpg, rule).createAndApply()
    )
    getRulesByNodeType(rules, NodeType.LEAKAGE.toString).foreach(rule => new LeakageTagger(cpg, rule).createAndApply())
    getRulesByNodeType(rules, NodeType.SDK.toString).foreach(rule => new SDKTagger(cpg, rule).createAndApply())
    getRulesByNodeType(rules, NodeType.API.toString).foreach(rule => new APITagger(cpg, rule).createAndApply())

    cpg.tag
  }

}
