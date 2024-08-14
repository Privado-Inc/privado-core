package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.{AppCache, FileLinkingMetadata, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.sink.api.{APISinkByMethodFullNameTagger, APISinkTagger}
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.Cpg

object JavaAPISinkTagger extends APISinkTagger {

  /** Wrapper method to tag all the api taggers
    * @param cpg
    * @param ruleCache
    */
  override def applyTagger(
    cpg: Cpg,
    ruleCache: RuleCache,
    privadoInput: PrivadoInput,
    appCache: AppCache,
    statsRecorder: StatsRecorder,
    fileLinkingMetadata: FileLinkingMetadata
  ): Unit = {

    super.applyTagger(cpg, ruleCache, privadoInput, appCache, statsRecorder, fileLinkingMetadata)
    new JavaAPIRetrofitTagger(cpg, ruleCache).createAndApply()

    if (privadoInput.enableAPIByParameter) {
      new JavaAPISinkByParameterMarkByAnnotationTagger(cpg, ruleCache).createAndApply()
      new JavaAPISinkByParameterTagger(cpg, ruleCache).createAndApply()
    }

    new APISinkByMethodFullNameTagger(cpg, ruleCache).createAndApply()

    new JavaAPITagger(cpg, ruleCache, privadoInput, appCache, statsRecorder, fileLinkingMetadata).createAndApply()
  }

}
