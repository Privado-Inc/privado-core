package ai.privado.languageEngine.ruby.tagger.sink

import ai.privado.cache.{AppCache, FileLinkingMetadata, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.sink.api.{APISinkByMethodFullNameTagger, APISinkTagger}
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg

object RubyAPISinkTagger extends APISinkTagger {

  override def applyTagger(
    cpg: Cpg,
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    appCache: AppCache,
    statsRecorder: StatsRecorder,
    fileLinkingMetadata: FileLinkingMetadata
  ): Unit = {

    super.applyTagger(cpg, ruleCache, privadoInput, appCache, statsRecorder, fileLinkingMetadata)

    new APISinkByMethodFullNameTagger(cpg, ruleCache).createAndApply()
    new APITagger(cpg, ruleCache, privadoInput = privadoInput, appCache, statsRecorder, fileLinkingMetadata)
      .createAndApply()
  }

}
