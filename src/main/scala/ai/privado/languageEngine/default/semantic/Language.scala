package ai.privado.languageEngine.default.semantic

import ai.privado.languageEngine.default.tagger.PrivadoTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object Language {

  implicit def tagger(cpg: Cpg): PrivadoTagger = new PrivadoTagger(cpg)

}
