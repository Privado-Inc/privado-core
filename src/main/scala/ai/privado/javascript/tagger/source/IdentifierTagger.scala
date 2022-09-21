package ai.privado.javascript.tagger.source

import ai.privado.model.InternalTag
import ai.privado.tagger.PrivadoSimplePass
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate

class IdentifierTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    val rulePattern = ruleInfo.patterns.head
    // val regexMatchingIdentifiers = cpg.identifier(rulePattern).l
    val regexMatchingIdentifiers = cpg.argument.isFieldIdentifier.code(rulePattern).l
    regexMatchingIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo)
    })
  }
}
