package ai.privado.tagger.sink

import ai.privado.tagger.PrivadoSimplePass
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities._

class CustomInheritTagger(cpg: Cpg) extends PrivadoSimplePass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    val typeDeclNode = cpg.typeDecl
      .filter(
        _.inheritsFromTypeFullName
          .map(inheritsFrom => inheritsFrom.matches(ruleInfo.patterns.head))
          .foldLeft(false)((a, b) => a || b)
      )
      .l
    if (typeDeclNode.nonEmpty) {
      typeDeclNode.fullName.dedup.foreach(typeDeclName => {
        val callNodes = cpg.call.methodFullName(typeDeclName + ".*" + ruleInfo.patterns(1)).l
        callNodes.foreach(callNode => addRuleTags(builder, callNode, ruleInfo))
      })
    }
  }
}
