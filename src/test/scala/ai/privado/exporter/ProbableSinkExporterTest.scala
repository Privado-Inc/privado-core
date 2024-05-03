package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache}
import better.files.File
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.exporter.ProbableSinkExporter
import ai.privado.model.ConfigAndRules
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg

import scala.collection.mutable

class ProbableSinkExporterTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private var inputDir: File                             = _
  var cpg: Cpg                                           = _
  val ruleCache                                          = new RuleCache()
  private val inputDirs                                  = mutable.ArrayBuffer.empty[File]
  private var probableSinkExporter: ProbableSinkExporter = _

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    val config  = Config().withInputPath(inputDir.pathAsString).withUseDeprecatedFrontend(true)
    val rubySrc = new RubySrc2Cpg()
    val xtocpg = rubySrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }
    cpg = xtocpg.get
    probableSinkExporter = new ProbableSinkExporter(cpg, ruleCache, "", appCache = new AppCache())
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    super.afterAll()
  }

  def getDirectoryPath(code: String, fileName: String): String = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / fileName).write(code)
    inputDir.toString()
  }

  "getProbableSinkForRuby" should {
    "return a list of probable sinks for a valid repository path" in {
      val repoPath = getDirectoryPath(
        """
          |gem sink1
          |gem sink2
          |gem sink3
          |""".stripMargin,
        "Gemfile"
      )
      val expectedSinks = List("sink1", "sink2", "sink3")

      val result = probableSinkExporter.getProbableSinkForRuby(repoPath)
      result should contain theSameElementsAs expectedSinks
    }

    "return an empty list when the repository path does not contain any Gemfiles" in {
      val repoPath = getDirectoryPath("".stripMargin, "package.json")

      val result = probableSinkExporter.getProbableSinkForRuby(repoPath)
      result shouldBe empty
    }

    "return an empty list when the repository path is invalid" in {
      val repoPath = "path/to/invalid/repository"

      val result = probableSinkExporter.getProbableSinkForRuby(repoPath)
      result shouldBe empty
    }

    "return a list of probable sinks for a repository path with Gemfiles containing probable sinks" in {
      val gemfileContents =
        """
          |gem 'rails', '4.2.8'
          |#Using database adapter as MYSQL2
          |gem 'mysql2', '~> 0.4.10'
          |gem 'haml'
          |gem 'rubocop', require: false
          |gem 'sdoc', '~> 0.4.0', group: :doc
          |#Using bootstrap sass to use bootstrap and its library
          |gem 'bootstrap-sass', '~> 3.3', '>= 3.3.7'
          |#Using bootstrap kind of pagination
          |gem 'bootstrap-will_paginate'
          |#Using carrier wave to upload image
          |gem 'carrierwave', '~> 1.2', '>= 1.2.2'
          |""".stripMargin
      val repoPath = getDirectoryPath(gemfileContents, "Gemfile")
      val expectedSinks =
        List("rails", "mysql2", "haml", "rubocop", "sdoc", "bootstrap-sass", "bootstrap-will_paginate", "carrierwave")

      val result = probableSinkExporter.getProbableSinkForRuby(repoPath)
      result should contain theSameElementsAs expectedSinks
    }

  }
}
