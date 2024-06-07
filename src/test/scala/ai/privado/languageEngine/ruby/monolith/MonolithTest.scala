package ai.privado.languageEngine.ruby.monolith

import ai.privado.cache.{
  AppCache,
  AuditCache,
  DataFlowCache,
  DatabaseDetailsCache,
  RuleCache,
  S3DatabaseDetailsCache,
  TaggerCache
}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.monolith.MonolithExporter
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.languageEngine.ruby.passes.config.RubyPropertyLinkerPass
import ai.privado.languageEngine.ruby.tagger.monolith.MonolithTagger
import ai.privado.model.{Constants, Language}
import ai.privado.rule.RuleInfoTestData
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.dataflowengineoss.language.Path
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

class MonolithTest extends MonolithTestBase {

  override val rubyFileContentMap: Map[String, String] = getContent

  def getContent: Map[String, String] = {
    val fileMap = mutable.HashMap[String, String]()

    val firstController =
      """
        |class FirstController < ApplicationController
        |  before_action :authenticate_user!
        |  def foo
        |     firstName = "rahul"
        |  end
        |end
        |""".stripMargin

    val secondController =
      """
        |class SecondController < ApplicationController
        |  before_action :authenticate_user!
        |end
        |""".stripMargin

    val generalCode =
      """
        |firstName = "rakesh"
        |""".stripMargin

    fileMap.put("first_controller.rb", firstController)
    fileMap.put("second_controller.rb", secondController)
    fileMap.put("general.rb", generalCode)
    fileMap.toMap
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, new TaggerCache).createAndApply()
    new MonolithTagger(cpg, ruleCache).createAndApply()
  }

  "Monolith tagger" should {
    "be able to add tag file nodes of controllers" in {
      cpg.file.where(_.tag.nameExact(Constants.monolithRepoItem)).size shouldBe 2
    }

    "be able to tag sources of repository items" in {
      cpg.identifier("firstName").where(_.tag.nameExact(Constants.monolithRepoItem)).size shouldBe 1
    }

    "be able to avoid tag of nonController files" in {
      val nonControllerNode = cpg.identifier("firstName").where(_.file.name(".*general.rb")).l
      nonControllerNode.size shouldBe 1
      nonControllerNode.tag.nameExact(Constants.monolithRepoItem).size shouldBe 0
    }
  }

  "Monolith Exporter" should {
    "be able to export individual privado.json" in {
      val privadoInput = PrivadoInput(isMonolith = true)
      // TODO Need to discard usage of AppCache as a static object and use it as a instance instead
      val appCache = new AppCache()
      appCache.repoLanguage = Language.RUBY
      val monolithJsonPaths = cpg.tag
        .nameExact(Constants.monolithRepoItem)
        .value
        .dedup
        .flatMap(repoItemName =>
          MonolithExporter.fileExport(
            cpg,
            repoItemName,
            Constants.outputFileName,
            "temporaryRubyFolder",
            Map[String, Path](),
            ruleCache,
            new TaggerCache(),
            new DataFlowCache(privadoInput, AuditCache()),
            privadoInput,
            s3DatabaseDetailsCache,
            appCache = appCache,
            databaseDetailsCache = DatabaseDetailsCache()
          )
        )
        .l
      monolithJsonPaths.size shouldBe 2
    }
  }
}

abstract class MonolithTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val rubyFileContentMap: Map[String, String]
  var inputDir: File         = _
  var output: File           = _
  val ruleCache              = new RuleCache()
  val s3DatabaseDetailsCache = new S3DatabaseDetailsCache()

  ruleCache.setRule(RuleInfoTestData.rule)

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- rubyFileContentMap) {
      (inputDir / key).write(content)
    }
    output = File.newTemporaryDirectory()

    val config =
      Config().withInputPath(inputDir.pathAsString).withOutputPath(output.pathAsString).withUseDeprecatedFrontend(true)
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
