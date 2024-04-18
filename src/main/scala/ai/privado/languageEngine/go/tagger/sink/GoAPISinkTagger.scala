package ai.privado.languageEngine.go.tagger.sink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.sink.api.APISinkTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object GoAPISinkTagger extends APISinkTagger {

  override def applyTagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput, appCache: AppCache): Unit = {

    super.applyTagger(cpg, ruleCache, privadoInput, appCache)

    new GoAPITagger(cpg, ruleCache, privadoInput, appCache).createAndApply()
  }

}
