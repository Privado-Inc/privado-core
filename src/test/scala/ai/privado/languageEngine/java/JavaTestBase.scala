package ai.privado.languageEngine.java

import ai.privado.languageEngine.ruby.RubyTestBase.code
import ai.privado.model.SourceCodeModel
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

object JavaTestBase {
  def code(sourceCodes: List[SourceCodeModel], applyPostProcessingPass: Boolean = false): (Cpg, Config) = {

    val (cpg, config) = code(sourceCodes)

    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)

    (cpg, config)
  }

  private def code(sourceCodes: List[SourceCodeModel]): (Cpg, Config) = {
    val inputDir = File.newTemporaryDirectory()
    for (sourceCode <- sourceCodes) {
      (inputDir / sourceCode.fileName).write(sourceCode.sourceCode)
    }
    val outputFile = File.newTemporaryFile()

    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
    val cpg    = new JavaSrc2Cpg().createCpg(config).get
    applyDefaultOverlays(cpg)
    (cpg, config)
  }
}
