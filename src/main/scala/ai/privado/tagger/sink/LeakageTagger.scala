package ai.privado.tagger.sink

import ai.privado.model.RuleInfo
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._

class LeakageTagger(cpg: Cpg, ruleInfo: RuleInfo) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val leakages = cpg.call.methodFullName(ruleInfo.pattern)

    leakages.foreach(leakage => addRuleTags(builder, leakage, ruleInfo))

  }
}
