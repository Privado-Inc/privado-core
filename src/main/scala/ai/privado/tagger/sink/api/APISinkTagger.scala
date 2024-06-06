package ai.privado.tagger.sink.api

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.Tagger
import io.shiftleft.codepropertygraph.generated.Cpg

trait APISinkTagger extends Tagger {

  override def applyTagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput, appCache: AppCache): Unit = {
    new InferenceAPIEndpointTagger(cpg, ruleCache).createAndApply()
  }

}
