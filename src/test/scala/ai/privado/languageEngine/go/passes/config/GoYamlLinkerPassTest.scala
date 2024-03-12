package ai.privado.languageEngine.go.passes.config

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.go.tagger.sink.GoAPITagger
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.model.{
  CatLevelOne,
  ConfigAndRules,
  Constants,
  FilterProperty,
  Language,
  NodeType,
  RuleInfo,
  SystemConfig
}
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*

class GoYamlLinkerPassTest extends GoYamlFilePassTestBase {
  override val yamlFileContents: String = """
      |config:
      |  default:
      |    API_URL: http://exampleKubernetesService
      |""".stripMargin

  override val codeFileContents: String = """
      |package main
      |
      |import (
      | "os"
      | "net/http"
      |)
      |
      |func main() {
      | api_url := os.getEnv("API_URL")
      | resp, err := http.Post(api_url)
      |}
      |""".stripMargin

  "Http client get API Sample" should {
    "Http client should be tagged" in {
      val callNode = cpg.call.name("Post").head
      callNode.tag.size shouldBe 6
      callNode.tag
        .nameExact(Constants.id)
        .head
        .value shouldBe (Constants.thirdPartiesAPIRuleId) + ".exampleKubernetesService"
      callNode.tag.nameExact(Constants.catLevelOne).head.value shouldBe Constants.sinks
      callNode.tag.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.third_parties
      callNode.tag.nameExact(Constants.nodeType).head.value shouldBe "api"
      callNode.tag
        .nameExact("third_partiesapi")
        .head
        .value shouldBe (Constants.thirdPartiesAPIRuleId + ".exampleKubernetesService")
      callNode.tag.nameExact("apiUrlSinks.ThirdParties.API.exampleKubernetesService")
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("config.default.API_URL")
        .contains("http://exampleKubernetesService")
    }

    "Two way edge between member and propertyNode" in {
      val properties = cpg.property.usedAt.originalProperty.l.map(property => (property.name, property.value)).toMap
      properties
        .get("config.default.API_URL")
        .contains("http://exampleKubernetesService") shouldBe true
    }
  }
}

abstract class GoYamlFilePassTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val yamlFileContents: String
  val codeFileContents: String
  var inputDir: File   = _
  var outputFile: File = _
  val ruleCache        = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "test.yaml").write(yamlFileContents)
    (inputDir / "GeneralConfig.go").write(codeFileContents)

    outputFile = File.newTemporaryFile()
    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)

    val goSrc = new GoSrc2Cpg()
    val xtocpg = goSrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

    cpg = xtocpg.get

    ruleCache.setRule(rule)

    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new PropertyParserPass(cpg, inputDir.toString(), new RuleCache, Language.GO).createAndApply()
    new GoYamlLinkerPass(cpg).createAndApply()
    new IdentifierTagger(cpg, ruleCache, TaggerCache()).createAndApply()
    new GoAPITagger(cpg, ruleCache, new PrivadoInput).createAndApply()
    super.beforeAll()
  }

  val systemConfig = List(
    SystemConfig(
      "apiHttpLibraries",
      "^(?i)(net/http|github.com/parnurzeal/gorequest|(gopkg.in|github.com/go-resty)/resty|valyala/fasthttp|github.com/gojektech/heimdall/v\\\\d/httpclient|github.com/levigross/grequests|github.com/PuerkitoBio/rehttp|github.com/machinebox/graphql).*",
      Language.GO,
      "",
      Array()
    ),
    SystemConfig(
      "apiSinks",
      "(?i)(?:url|client|open|request|execute|newCall|load|host|access|list|set|put|post|proceed|trace|patch|Path|send|remove|delete|write|read|postForEntity|call|createCall|createEndpoint|dispatch|invoke|getInput|getOutput|getResponse|do)",
      Language.GO,
      "",
      Array()
    ),
    SystemConfig(
      "apiIdentifier",
      "(?i).*((hook|base|auth|prov|endp|install|request|service|gateway|route|resource)(.){0,12}url|(slack|web)(.){0,4}hook|(rest|api|request|service)(.){0,4}(endpoint|gateway|route)).*",
      Language.GO,
      "",
      Array()
    )
  )

  val sinkRule = List(
    RuleInfo(
      Constants.thirdPartiesAPIRuleId,
      "Third Party API",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(
        "(?i)((?:http|https):\\/\\/[a-zA-Z0-9_-][^)\\/(#|,!>\\s]{1,50}\\.\\b(?:com|net|org|de|in|uk|us|io|gov|cn|ml|ai|ly|dev|cloud|me|icu|ru|info|top|tk|tr|cn|ga|cf|nl)\\b).*(?<!png|jpeg|jpg|txt|blob|css|html|js|svg)",
        "(?i).*((hook|base|auth|prov|endp|install|request|service|gateway|route|resource)(.){0,12}url|(slack|web)(.){0,4}hook|(rest|api|request|service)(.){0,4}(endpoint|gateway|route)).*"
      ),
      false,
      "",
      Map(),
      NodeType.API,
      "",
      CatLevelOne.SINKS,
      catLevelTwo = Constants.third_parties,
      Language.GO,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(List(), sinkRule, List(), List(), List(), List(), List(), List(), systemConfig, List())

}
