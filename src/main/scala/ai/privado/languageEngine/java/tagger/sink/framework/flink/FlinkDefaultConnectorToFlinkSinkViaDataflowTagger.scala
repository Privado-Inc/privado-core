package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.dataflow.{Dataflow, DuplicateFlowProcessor}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.tagger.PrivadoSimpleCpgPass
import scala.util.Try
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.semanticcpg.language.*

class FlinkDefaultConnectorToFlinkSinkViaDataflowTagger(
  cpg: Cpg,
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  appCache: AppCache
) extends PrivadoSimpleCpgPass(cpg)
    with TaggerHelper {

  override def run(builder: DiffGraphBuilder): Unit = {

    val flinkDefaultConnectors = getFlinkDefaultConnectors(cpg, ruleCache)

    val flinkSinks = cpg.call(flinkSinkName).l

    val paths = Dataflow.dataflowForSourceSinkPair(flinkDefaultConnectors, flinkSinks, privadoInput, appCache)
    DuplicateFlowProcessor.getUniquePathsAfterDedup(paths).foreach { path =>
      Try {
        val source = path.elements.head
        val sink   = path.elements.last
        copyTags(builder, ruleCache, List(source), sink)
      }
    }
  }
}
