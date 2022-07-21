package ai.privado.tagger

import ai.privado.model.RuleInfo
import ai.privado.tagger.source.{IdentifierTagger, LiteralTagger}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.NewTagNodePairTraversal
import overflowdb.traversal._
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._


class PrivadoTagger(cpg: Cpg) {


  def runTagger(rules : List[RuleInfo], sourceIds: String = "", sinkIds: String = "") : Traversal[Tag] = {

    for (rule <- rules){
      new LiteralTagger(cpg, rule).createAndApply()
      new IdentifierTagger(cpg, rule).createAndApply()
    }
    cpg.tag
  }


}
