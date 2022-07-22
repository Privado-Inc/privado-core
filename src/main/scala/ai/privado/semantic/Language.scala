package ai.privado.semantic

import ai.privado.tagger.PrivadoTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object Language {

  implicit def tagger(cpg: Cpg) : PrivadoTagger = new PrivadoTagger(cpg)

}
