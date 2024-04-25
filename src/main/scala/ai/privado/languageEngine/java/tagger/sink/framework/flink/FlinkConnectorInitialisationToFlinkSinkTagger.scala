package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.dataflow.{Dataflow, DuplicateFlowProcessor}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.InternalTag
import ai.privado.tagger.PrivadoSimpleCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language.*

/** This tagger focuses on getting the mapping for the connector to the `flinkSink` by doing dataflow operation
  * @param cpg
  * @param ruleCache
  * @param privadoInput
  * @param appCache
  */
class FlinkConnectorInitialisationToFlinkSinkTagger(
  cpg: Cpg,
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  appCache: AppCache
) extends PrivadoSimpleCpgPass(cpg)
    with TaggerHelper {

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {

    val source = cpg.local.where(_.tag.nameExact(InternalTag.FLINK_INITIALISATION_LOCAL_NODE.toString)).l

    val sink = cpg.call(flinkSinkName).l

    val paths = Dataflow.dataflowForSourceSinkPair(source, sink, privadoInput, appCache)
    DuplicateFlowProcessor.getUniquePathsAfterDedup(paths).foreach { path =>
      val source = path.elements.head
      val sink   = path.elements.last
      copyTags(builder, ruleCache, List(source), sink)
    }
  }
}
