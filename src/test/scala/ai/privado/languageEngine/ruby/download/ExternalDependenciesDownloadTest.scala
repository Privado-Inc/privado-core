package ai.privado.languageEngine.ruby.download

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.passes.download.DownloadDependenciesPass
import better.files.File
import io.joern.rubysrc2cpg.deprecated.utils.{ModuleModel, PackageTable, TypeDeclModel}
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class ExternalDependenciesDownloadTest extends ExternalDependenciesDownloadTestBase {

  override val rubyFileContentMap: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val testGemMap = mutable.HashMap[String, String]()

    val gemfileContent =
      """
        |source 'https://rubygems.org'
        |gem 'dummy_logger'
        |gem 'redis', '~> 5.0'
        |
        |""".stripMargin

    testGemMap.put("Gemfile", gemfileContent)
    testGemMap.toMap
  }

  "Ruby External Dependency Download" should {
    // TODO Due to this test case of downloading dependency, we are observing code build pipeline getting timed out, ignoring this till we figure out the issue
    "Test dummy_logger dependency download" ignore {

      val packageTable =
        new DownloadDependenciesPass(new PackageTable(), inputDir.pathAsString, ruleCache).createAndApply()

      // Should have dummy_logger as module
      packageTable.getModule("dummy_logger") shouldBe List(
        ModuleModel("Main_module", "dummy_logger::program.Main_module")
      )

      // should capture typeDecls
      packageTable.getTypeDecl("dummy_logger") shouldBe List(
        TypeDeclModel("Main_outer_class", "dummy_logger::program.Main_module.Main_outer_class"),
        TypeDeclModel("Help", "dummy_logger::program.Help")
      )
    }

    // TODO Remove this test case as anyways currently we are not focusing on methods
    "Test redis dependency download" ignore {
      val packageUsed = List("redis")

      val packageTable =
        new DownloadDependenciesPass(new PackageTable(), inputDir.pathAsString, ruleCache).createAndApply()

      packageTable.getMethodFullNameUsingName(packageUsed, "zscan_each") shouldBe List(
        "redis::program.Redis.Commands.SortedSets.zscan_each"
      )
    }
  }
}

abstract class ExternalDependenciesDownloadTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val rubyFileContentMap: Map[String, String]
  var inputDir: File = _
  var output: File   = _
  val ruleCache      = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- rubyFileContentMap) {
      (inputDir / key).write(content)
    }
    output = File.newTemporaryDirectory()

    val config  = Config().withInputPath(inputDir.pathAsString).withOutputPath(output.pathAsString)
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
