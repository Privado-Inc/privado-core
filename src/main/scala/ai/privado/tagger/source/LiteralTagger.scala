package ai.privado.tagger.source

import ai.privado.model.InternalTag
import ai.privado.tagger.PrivadoSimplePass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate
import ai.privado.utility.Utilities._

class LiteralTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    // Step 1.2
    val literals = cpg.literal.code(ruleInfo.patterns.head).l
    literals.foreach(literal => {
      storeForTag(builder, literal)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
      addRuleTags(builder, literal, ruleInfo)
    })
  }

}
