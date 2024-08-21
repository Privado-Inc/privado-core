package ai.privado.tagger

import ai.privado.cache.{
  AppCache,
  DataFlowCache,
  DatabaseDetailsCache,
  FileLinkingMetadata,
  RuleCache,
  S3DatabaseDetailsCache,
  TaggerCache
}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.nodes.Tag

abstract class PrivadoBaseTagger {

  def runTagger(rules: RuleCache): Traversal[Tag]                           = ???
  def runTagger(rules: RuleCache, taggerCache: TaggerCache): Traversal[Tag] = ???

  def runTagger(
    rules: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder,
    fileLinkingMetadata: FileLinkingMetadata
  ): Iterator[Tag] = ???

  def runTagger(
    rules: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    databaseDetailsCache: DatabaseDetailsCache,
    statsRecorder: StatsRecorder,
    fileLinkingMetadata: FileLinkingMetadata
  ): Iterator[Tag] = ???

}
