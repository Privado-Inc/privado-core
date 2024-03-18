package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.RuleInfoTestData
import ai.privado.cache.RuleCache
import ai.privado.entrypoint.PrivadoInput
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.languageEngine.java.JavaTestBase.*
import ai.privado.languageEngine.java.tagger.sink.JavaAPITagger
import ai.privado.model.{CatLevelOne, Constants, InternalTag, NodeType, SourceCodeModel}
import io.shiftleft.semanticcpg.language.*

class JavaAPISinkByParameterTaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "Api by matching a variable like parameter" should {
    "be tagged as a API sink" in {

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
          |        return new Client(endpoint);
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
          |        this.client = new Client(endpoint);
          |        this.endpointClient = new EndpointClient(config);
          |    }
          |
          |    public List<String> getAllDetails() {
          |        String url = "https://www.myproduction.com/user";
          |
          |        return client.getAllDetails(url); // This should be marked as API Sink by url like matching
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
      ruleCache.setRule(RuleInfoTestData.rule.copy())
      JavaAPISinkTagger.applyTagger(cpg, ruleCache = RuleCache(), privadoInput = privadoInput)

      new JavaAPITagger(cpg, ruleCache, privadoInputConfig = privadoInput).createAndApply()

      val apiSink = cpg.call("getAllDetails").l
      apiSink.tag.nameExact(InternalTag.API_SINK_MARKED.toString).size shouldBe 1
      apiSink.tag.nameExact(Constants.catLevelOne).value.headOption shouldBe Some(CatLevelOne.SINKS.name)
      apiSink.tag.nameExact(Constants.nodeType).value.headOption shouldBe Some(NodeType.API.toString)

      val apiSinkByEndpoint = cpg.call("getDetailsByEndpoint").l
      apiSinkByEndpoint.tag.nameExact(InternalTag.API_SINK_MARKED.toString).size shouldBe 1
      apiSinkByEndpoint.tag.nameExact(Constants.catLevelOne).value.headOption shouldBe Some(CatLevelOne.SINKS.name)
      apiSinkByEndpoint.tag.nameExact(Constants.nodeType).value.headOption shouldBe Some(NodeType.API.toString)
    }
  }

}
