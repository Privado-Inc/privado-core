package ai.privado.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import io.shiftleft.codepropertygraph.generated.Cpg

trait APISinkTagger {

  def applyTagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput): Unit = ???

}
