package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.languageEngine.java.JavaTestBase.*
import ai.privado.model.{CatLevelOne, Constants, InternalTag, Language, NodeType, SourceCodeModel, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import io.shiftleft.semanticcpg.language.*

class JavaAPISinkByParameterTaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "Api by matching a variable like parameter" should {
    // Ignoring this, as we have deprecated Parameter Tagger Test
    "be tagged as a API sink" ignore {

      val (cpg, config) = code(
        List(
          SourceCodeModel(
            """
          |import java.util.List;
          |import ai.privado.client.Client;
          |
          |public class EndpointClient {
          |
          |    private String config;
          |
          |    public EndpointClient(String config) {
          |         this.config = config;
          |    }
          |
          |
          |    public Client getClient(String endpoint) {
          |        // Logic to create and return a client based on the endpoint
          |        Client client = new Client(endpoint);
          |        return client;
          |    }
          |}
          |
          |""".stripMargin,
            "EndpointClient.java"
          ),
          SourceCodeModel(
            """
          |import java.util.List;
          |import ai.privado.client.Client;
          |
          |public class Main {
          |    private Client client;
          |
          |    private EndpointClient endpointClient;
          |
          |    public Main(String endpoint, String config) {
          |        this.client = new Client();
          |        this.endpointClient = new EndpointClient(config);
          |    }
          |
          |    public List<String> getAllDetails() {
          |
          |
          |        return client.getAllDetails(); // This should be marked as API Sink by url like matching
          |    }
          |
          |    public List<String> getDetailsByEndpoint() {
          |         String url = "https://www.myproduction.com/user/endpoint";
          |
          |         return endpointClient.getDetailsByEndpoint(url); // This should be marked as API Sink by config like matching
          |
          |    }
          |}
          |""".stripMargin,
            "Main.java"
          )
        )
      )

      val privadoInput = PrivadoInput(enableAPIByParameter = true)
      val ruleCache    = RuleCache()
      val systemConfig =
        List(SystemConfig(Constants.apiIdentifier, "(?i).*endpoint.*", Language.UNKNOWN, "", Array[String]()))
      ruleCache.setRule(RuleInfoTestData.rule.copy(systemConfig = systemConfig))
      JavaAPISinkTagger.applyTagger(cpg, ruleCache = ruleCache, privadoInput = privadoInput, appCache = AppCache())

      new JavaAPITagger(cpg, ruleCache, privadoInputConfig = privadoInput, appCache = new AppCache()).createAndApply()

      val apiSink = cpg.call("getAllDetails").l
      apiSink.tag.nameExact(InternalTag.API_SINK_MARKED.toString).size shouldBe 1
      apiSink.tag.nameExact(Constants.catLevelOne).value.headOption shouldBe Some(CatLevelOne.SINKS.name)
      apiSink.tag.nameExact(Constants.nodeType).value.headOption shouldBe Some(NodeType.API.toString)

      apiSink.tag.nameExact(Constants.id).value.l shouldBe List("Sinks.ThirdParties.API.endpoint")
      apiSink.tag.nameExact(Constants.apiUrl + "Sinks.ThirdParties.API.endpoint").value.l shouldBe List("endpoint")

      val apiSinkByEndpoint = cpg.call("getDetailsByEndpoint").l
      apiSinkByEndpoint.tag.nameExact(InternalTag.API_SINK_MARKED.toString).size shouldBe 1
      apiSinkByEndpoint.tag.nameExact(Constants.catLevelOne).value.headOption shouldBe Some(CatLevelOne.SINKS.name)
      apiSinkByEndpoint.tag.nameExact(Constants.nodeType).value.headOption shouldBe Some(NodeType.API.toString)
    }
  }

}
