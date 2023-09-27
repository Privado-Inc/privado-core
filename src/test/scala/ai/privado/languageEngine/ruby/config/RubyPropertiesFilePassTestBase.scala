package ai.privado.languageEngine.ruby.config

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.passes.config.RubyPropertyLinkerPass
import ai.privado.model.Language
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class RubyPropertiesFilePassTestBase(fileExtension: String)
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll {
  var cpg: Cpg = _
  val configFileContents: String
  val codeFileContents: String
  var inputDir: File  = _
  var outputDir: File = _

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / s"config$fileExtension").write(configFileContents)
    (inputDir / s"code.rb").write(codeFileContents)
    outputDir = File.newTemporaryDirectory()

    val config  = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputDir.pathAsString)
    val rubySrc = new RubySrc2Cpg()
    val xtocpg = rubySrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

    cpg = xtocpg.get
    new PropertyParserPass(cpg, inputDir.pathAsString, new RuleCache(), Language.RUBY).createAndApply()
    new RubyPropertyLinkerPass(cpg).createAndApply()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputDir.delete()
    super.afterAll()
  }
}
