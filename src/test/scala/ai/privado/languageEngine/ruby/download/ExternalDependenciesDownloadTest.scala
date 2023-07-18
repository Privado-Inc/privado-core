package ai.privado.languageEngine.ruby.download

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.cache.PackageTable
import better.files.File
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

  /*
  "Ruby External Dependency Download" ignore {
    "Test dummy_logger dependency download" in {
      val packageUsed = List("dummy_logger")

      val packageTable = ExternalDependenciesResolver.downloadDependencies(cpg, inputDir.pathAsString)

      // Should have dummy_logger as module
      packageTable.containsModule("dummy_logger") shouldBe true

      // should have methodName
      packageTable.getMethodFullNameUsingName(packageUsed, "third_fun").get should equal(
        "dummy_logger::program.Main_module.Main_outer_class.third_fun"
      )
      packageTable.getMethodFullNameUsingName(packageUsed, "first_fun").get should equal(
        "dummy_logger::program.Main_module.Main_outer_class.first_fun"
      )
      packageTable.getMethodFullNameUsingName(packageUsed, "help_print").get should equal(
        "dummy_logger::program.Help.help_print"
      )
    }

    "Test redis dependency download" in {
      val packageUsed = List("redis")

      val packageTable = ExternalDependenciesResolver.downloadDependencies(cpg, inputDir.pathAsString)

      packageTable.containsModule("redis") shouldBe true

      packageTable.getMethodFullNameUsingName(packageUsed, "zscan_each").get should equal(
        "redis::program.Redis.Commands.SortedSets.zscan_each"
      )
    }
  }
   */
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
