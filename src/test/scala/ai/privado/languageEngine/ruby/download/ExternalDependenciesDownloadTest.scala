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
        |
        |""".stripMargin

    testGemMap.put("Gemfile", gemfileContent)
    testGemMap.toMap
  }

  "Ruby External Dependency Download" should {
    "Test dependency download" in {
      val packageTable = new PackageTable()
      val packageUsed = List("dummy_logger")

      ExternalDependenciesResolver.downloadDependencies(cpg, inputDir.pathAsString, packageTable)

      // Should have dummy_logger as module
      packageTable.containsModule("dummy_logger") shouldBe true

      // should have methodName
      packageTable.getMethodFullNameUsingName(packageUsed, "third_fun").get should equal("dummy_logger::program:Main_module:Main_outer_class:third_fun")
      packageTable.getMethodFullNameUsingName(packageUsed, "first_fun").get should equal("dummy_logger::program:Main_module:Main_outer_class:first_fun")
      packageTable.getMethodFullNameUsingName(packageUsed, "help_print").get should equal("dummy_logger::program:Help:help_print")
    }
  }
}
