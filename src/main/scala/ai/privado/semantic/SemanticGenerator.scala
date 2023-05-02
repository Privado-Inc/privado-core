package ai.privado.semantic

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.languageEngine.python.passes.PythonSemanticGenerator
import ai.privado.model.Language
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg

object SemanticGenerator {

  /** Utility to get the default semantics for dataflow queries
    *
    * @return
    */
  def getDefaultSemantics: Semantics = {
    DefaultSemantics()
  }

  def getSemantics(cpg: Cpg, privadoScanConfig: PrivadoInput, ruleCache: RuleCache): Semantics = {
    val lang = AppCache.repoLanguage

    lang match {
      case Language.JAVA   => JavaSemanticGenerator.getSemantics(cpg, privadoScanConfig, ruleCache)
      case Language.PYTHON => PythonSemanticGenerator.getSemantics(cpg, privadoScanConfig, ruleCache)
      case _               => getDefaultSemantics
    }

  }
}
