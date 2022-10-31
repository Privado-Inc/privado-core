package ai.privado.tagger

import ai.privado.model.ConfigAndRules
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import overflowdb.traversal.Traversal

abstract class PrivadoBaseTagger {

  def runTagger(rules: ConfigAndRules): Traversal[Tag] = ???

}
