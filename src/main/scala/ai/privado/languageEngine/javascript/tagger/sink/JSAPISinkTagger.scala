package ai.privado.languageEngine.javascript.tagger.sink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.sink.api.{APISinkByMethodFullNameTagger, APISinkTagger}
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg

object JSAPISinkTagger extends APISinkTagger {

  override def applyTagger(
    cpg: Cpg,
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    appCache: AppCache,
    statsRecorder: StatsRecorder
  ): Unit = {

    super.applyTagger(cpg, ruleCache, privadoInput, appCache, statsRecorder)

    new APISinkByMethodFullNameTagger(cpg, ruleCache).createAndApply()

    new JSAPITagger(cpg, ruleCache, privadoInput = privadoInput, appCache = appCache).createAndApply()

    new GraphqlAPITagger(cpg, ruleCache).createAndApply()

  }

}
