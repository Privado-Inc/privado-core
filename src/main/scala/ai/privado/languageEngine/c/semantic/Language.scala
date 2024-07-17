package ai.privado.languageEngine.c.semantic

import ai.privado.languageEngine.c.tagger.PrivadoTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object Language {
  implicit def tagger(cpg: Cpg): PrivadoTagger = new PrivadoTagger(cpg)
}
