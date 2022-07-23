package ai.privado.tagger.sink

import ai.privado.model.RuleInfo
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities._

class DatabaseTagger(cpg: Cpg, ruleInfo: RuleInfo) extends SimpleCpgPass(cpg) {
  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val databases = cpg.call.methodFullName(ruleInfo.pattern)

    databases.foreach(database => addRuleTags(builder, database, ruleInfo))

  }
}
