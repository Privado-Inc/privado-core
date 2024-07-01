package ai.privado.languageEngine.csharp.tagger.sink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.sink.api.APISinkTagger
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg

object CSharpAPISinkTagger extends APISinkTagger {

  override def applyTagger(
    cpg: Cpg,
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    appCache: AppCache,
    statsRecorder: StatsRecorder
  ): Unit = {

    super.applyTagger(cpg, ruleCache, privadoInput, appCache, statsRecorder)

    new CSharpAPITagger(cpg, ruleCache, privadoInput, appCache).createAndApply()
  }

}
