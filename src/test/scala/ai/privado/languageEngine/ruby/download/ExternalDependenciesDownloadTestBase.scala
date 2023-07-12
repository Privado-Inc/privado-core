package ai.privado.languageEngine.ruby.download

import ai.privado.cache.RuleCache
import better.files.File
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


abstract class ExternalDependenciesDownloadTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val rubyFileContentMap: Map[String, String]
  var inputDir: File = _
  var output: File = _
  val ruleCache = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- rubyFileContentMap) {
      (inputDir / key).write(content)
    }
    output = File.newTemporaryDirectory()

    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(output.pathAsString)
    val rubySrc = new RubySrc2Cpg()
    val xtocpg = rubySrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

    cpg = xtocpg.get
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    output.delete()
    super.afterAll()
  }
}
