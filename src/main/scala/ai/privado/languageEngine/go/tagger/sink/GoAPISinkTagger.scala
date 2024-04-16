package ai.privado.languageEngine.go.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.sink.api.APISinkTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object GoAPISinkTagger extends APISinkTagger {

  override def applyTagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput): Unit =
    super.applyTagger(cpg, ruleCache, privadoInput)

}
