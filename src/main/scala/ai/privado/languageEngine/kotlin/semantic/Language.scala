package ai.privado.languageEngine.kotlin.semantic

import ai.privado.languageEngine.kotlin.tagger.PrivadoTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object Language {

  implicit def tagger(cpg: Cpg): PrivadoTagger = new PrivadoTagger(cpg)

}
