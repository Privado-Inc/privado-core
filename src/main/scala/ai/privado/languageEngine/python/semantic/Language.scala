package ai.privado.languageEngine.python.semantic

import ai.privado.languageEngine.python.tagger.PrivadoTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object Language {

  implicit def tagger(cpg: Cpg): PrivadoTagger = new PrivadoTagger(cpg)

}
