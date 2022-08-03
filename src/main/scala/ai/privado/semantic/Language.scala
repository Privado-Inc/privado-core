package ai.privado.semantic

import ai.privado.dataflow.Dataflow
import ai.privado.tagger.PrivadoTagger
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.{DefaultNodeExtensionFinder, NodeExtensionFinder}

object Language {

  implicit def tagger(cpg: Cpg): PrivadoTagger     = new PrivadoTagger(cpg)
  implicit def privadoDataflow(cpg: Cpg): Dataflow = new Dataflow(cpg)

  implicit val finder: NodeExtensionFinder  = DefaultNodeExtensionFinder
  implicit val engineContext: EngineContext = EngineContext(Utilities.getDefaultSemantics)

}
