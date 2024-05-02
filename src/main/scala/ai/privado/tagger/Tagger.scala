package ai.privado.tagger

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import io.shiftleft.codepropertygraph.generated.Cpg

trait Tagger {

  def applyTagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput, appCache: AppCache): Unit = ???

}
