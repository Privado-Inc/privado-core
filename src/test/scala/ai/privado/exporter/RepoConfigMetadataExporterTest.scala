package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.model.{ConfigAndRules, Language, SystemConfig}
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.wordspec.AnyWordSpec

class RepoConfigMetadataExporterTest extends RepoConfigMetadataExporterBase {
  override val yamlFileContents: String = """
      |name: exampleService
      |
      |config:
      |  prod:
      |    DB_HOST_NAME: example.com
      |""".stripMargin

  "Test Repo config Metadata sample" should {
    "should return correct metadata" in {
      val resultMap = RepoConfigMetaDataExporter.getMetaData(cpg, ruleCache).toMap
      resultMap.keys.toList should contain("name")
      resultMap("name") should equal("exampleService")

      resultMap.keys.toList should contain("config.prod.DB_HOST_NAME")
      resultMap("config.prod.DB_HOST_NAME") should equal("example.com")
    }
  }
}

abstract class RepoConfigMetadataExporterBase
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach {
  var cpg: Cpg = _
  val yamlFileContents: String
  var inputDir: File   = _
  var outputFile: File = _
  val ruleCache        = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "test.yaml").write(yamlFileContents)

    outputFile = File.newTemporaryFile()
    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)

    cpg = new JavaSrc2Cpg()
      .createCpg(config)
      .map { cpg =>
        applyDefaultOverlays(cpg)
        cpg
      }
      .get

    ruleCache.setRule(rule)
    new PropertyParserPass(cpg, inputDir.toString(), new RuleCache, Language.JAVA).createAndApply()

    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

  val systemConfig = List(
    SystemConfig(
      "RepoPropertyConfig",
      "name|config.prod.DB_HOST_NAME",
      Language.JAVA,
      "",
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(systemConfig = systemConfig)
}
