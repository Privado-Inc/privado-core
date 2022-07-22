package ai.privado.tagger

import ai.privado.model.RuleInfo
import ai.privado.tagger.source.{IdentifierTagger, LiteralTagger}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{Traversal}



class PrivadoTagger(cpg: Cpg){

  def runTagger(rules : List[RuleInfo]) : Traversal[Tag] = {

    for (rule <- rules){
      new LiteralTagger(cpg, rule).createAndApply()
      new IdentifierTagger(cpg, rule).createAndApply()
    }
    cpg.tag
  }


}
