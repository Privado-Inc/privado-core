package ai.privado.javascript.semantic

//import ai.privado.java.dataflow.Dataflow
import ai.privado.javascript.tagger.PrivadoTagger
import io.shiftleft.codepropertygraph.generated.Cpg

object Language {

  implicit def tagger(cpg: Cpg): PrivadoTagger = new PrivadoTagger(cpg)
  // implicit def privadoDataflow(cpg: Cpg): Dataflow = new Dataflow(cpg)

}
