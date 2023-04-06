package ai.privado.tagger

import ai.privado.cache.TaggerCache
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.ConfigAndRules
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import overflowdb.traversal.Traversal

abstract class PrivadoBaseTagger {

  def runTagger(rules: ConfigAndRules): Traversal[Tag]                                                             = ???
  def runTagger(rules: ConfigAndRules, taggerCache: TaggerCache): Traversal[Tag]                                   = ???
  def runTagger(rules: ConfigAndRules, taggerCache: TaggerCache, privadoInputConfig: PrivadoInput): Traversal[Tag] = ???

}
