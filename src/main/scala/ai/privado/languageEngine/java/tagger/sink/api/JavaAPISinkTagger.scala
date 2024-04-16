package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.tagger.sink.JavaAPIRetrofitTagger
import ai.privado.tagger.sink.api.APISinkTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object JavaAPISinkTagger extends APISinkTagger {

  /** Wrapper method to tag all the api taggers
    * @param cpg
    * @param ruleCache
    */
  override def applyTagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput): Unit = {

    super.applyTagger(cpg, ruleCache, privadoInput)
    new JavaAPIRetrofitTagger(cpg, ruleCache).createAndApply()

    if (privadoInput.enableAPIByParameter) {
      new JavaAPISinkByParameterMarkByAnnotationTagger(cpg, ruleCache).createAndApply()
      new JavaAPISinkByParameterTagger(cpg, ruleCache).createAndApply()
    }

    new JavaAPISinkByMethodFullNameTagger(cpg, ruleCache).createAndApply()
  }

}
