package ai.privado.tagger

import ai.privado.cache.{DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import overflowdb.traversal.Traversal

abstract class PrivadoBaseTagger {

  def runTagger(rules: RuleCache): Traversal[Tag]                           = ???
  def runTagger(rules: RuleCache, taggerCache: TaggerCache): Traversal[Tag] = ???
  def runTagger(
    rules: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache
  ): Traversal[Tag] = ???

}
