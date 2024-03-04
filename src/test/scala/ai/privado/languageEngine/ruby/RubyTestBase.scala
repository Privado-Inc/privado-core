package ai.privado.languageEngine.ruby

import ai.privado.languageEngine.ruby.passes.{GlobalImportPass, PrivadoRubyTypeRecoveryPassGenerator}
import ai.privado.model.SourceCodeModel
import io.shiftleft.codepropertygraph.generated.Cpg
import better.files.File
import io.joern.rubysrc2cpg.deprecated.passes.RubyTypeHintCallLinker
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.passes.frontend.{LocalKey, SBKey, SymbolTable}
import io.shiftleft.semanticcpg.language.*
object RubyTestBase {

  def code(sourceCodes: List[SourceCodeModel], applyPostProcessingPass: Boolean = false): (Cpg, Config) = {

    val (cpg, config) = code(sourceCodes)
    if (applyPostProcessingPass) {
      val globalSymbolTable = new SymbolTable[LocalKey](SBKey.fromNodeToLocalKey)
      new GlobalImportPass(cpg, globalSymbolTable).createAndApply()

      new PrivadoRubyTypeRecoveryPassGenerator(cpg, globalSymbolTable).generate().foreach(_.createAndApply())
      new RubyTypeHintCallLinker(cpg).createAndApply()
    }
    (cpg, config)
  }
  private def code(sourceCodes: List[SourceCodeModel]): (Cpg, Config) = {
    val inputDir = File.newTemporaryDirectory()
    for (sourceCode <- sourceCodes) {
      (inputDir / sourceCode.fileName).write(sourceCode.sourceCode)
    }
    val outputFile = File.newTemporaryFile()
    val config = Config()
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)
      .withUseDeprecatedFrontend(true)
    val rubySrc = new RubySrc2Cpg()
    val xtocpg = rubySrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }
    (xtocpg.get, config)
  }
}
