package ai.privado.languageEngine.java.passes.config

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.tagger.sink.JavaAPITagger
import ai.privado.languageEngine.java.tagger.source.IdentifierTagger
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
import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.should
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*

class JavaYamlLinkerPassTest extends JavaYamlLinkerPassTestBase {
  override val yamlFileContents =
    """
      |config:
      |  default:
      |    API_URL: http://exampleKubernetesService
      |""".stripMargin

  override val codeFileContents =
    """
      |import org.apache.http.HttpResponse;
      |import org.apache.http.client.HttpClient;
      |import org.apache.http.client.methods.HttpGet;
      |import org.apache.http.impl.client.HttpClients;
      |
      |public class APICaller {
      |  private String apiUrl;
      |
      |  public makeCall() {
      |    apiUrl = System.getenv("API_URL");
      |
      |    HttpClient httpClient = HttpClients.createDefault();
      |    HttpGet getRequest = new HttpGet(apiUrl);
      |    HttpResponse response = httpClient.execute(getRequest);
      |  }
      |}
      |""".stripMargin

  "Http client execute API Sample" should {
    "Http Client execute should be tagged" in {
      val callNode = cpg.call.name("execute").head
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

abstract class JavaYamlLinkerPassTestBase
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach {
  var cpg: Cpg = _
  val yamlFileContents: String
  val codeFileContents: String
  var inputDir: File   = _
  var outputFile: File = _
  val ruleCache        = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "test.yaml").write(yamlFileContents)
    (inputDir / "GeneralConfig.java").write(codeFileContents)

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

    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new PropertyParserPass(cpg, inputDir.toString(), new RuleCache, Language.JAVA).createAndApply()
    new JavaPropertyLinkerPass(cpg).createAndApply()
    new JavaYamlLinkerPass(cpg).createAndApply()
    new IdentifierTagger(cpg, ruleCache, TaggerCache()).createAndApply()
    new JavaAPITagger(cpg, ruleCache, PrivadoInput()).createAndApply()

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
      "apiHttpLibraries",
      "(?i)(org.apache.http|okhttp|org.glassfish.jersey|com.mashape.unirest|java.net.http|java.net.URL|org.springframework.(web|core.io)|groovyx.net.http|org.asynchttpclient|kong.unirest.java|org.concordion.cubano.driver.http|javax.net.ssl|javax.xml.soap|org.apache.axis2|com.sun.xml.messaging.saaj|org.springframework.ws.client|com.eviware.soapui|org.apache.cxf|org.jboss.ws|com.ibm.websphere.sca.extensions.soap|com.sun.xml.ws|org.apache.camel.component.cxf|org.codehaus.xfire|org.apache.synapse|org.apache.wink.client|com.oracle.webservices.internal.api.databinding.Databinding|com.sap.engine.interfaces.webservices.runtime.client).*",
      Language.JAVA,
      "",
      Array()
    ),
    SystemConfig(
      "apiSinks",
      "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|fetch|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity|postForObject|call|createCall|createEndpoint|dispatch|invoke|newMessage|getInput|getOutput|getResponse|marshall|unmarshall|send|asyncSend)",
      Language.JAVA,
      "",
      Array()
    ),
    SystemConfig(
      "apiIdentifier",
      "(?i).*((hook|base|auth|prov|endp|install|request|service|gateway|route|resource)(.){0,12}url|(slack|web)(.){0,4}hook|(rest|api|request|service)(.){0,4}(endpoint|gateway|route)).*",
      Language.JAVA,
      "",
      Array()
    ),
    SystemConfig(
      "ignoredSinks",
      "(?i).*(?<=map|list|jsonobject|json|array|arrays|jsonnode|objectmapper|objectnode).*(put:|get:).*",
      Language.JAVA,
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
      Language.JAVA,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(List(), sinkRule, List(), List(), List(), List(), List(), List(), systemConfig, List())
}
