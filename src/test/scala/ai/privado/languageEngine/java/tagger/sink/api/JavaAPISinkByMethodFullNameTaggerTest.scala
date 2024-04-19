package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.languageEngine.java.JavaTestBase.*
import ai.privado.model.{ConfigAndRules, Constants, InternalTag, Language, SourceCodeModel, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import io.shiftleft.semanticcpg.language.*

class JavaAPISinkByMethodFullNameTaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "call which match api methodFullName regex" should {
    "match" in {

      val (cpg, config) = code(
        List(
          SourceCodeModel(
            """
          |
          |import java.io.BufferedReader;
          |import java.io.IOException;
          |import java.io.InputStreamReader;
          |import java.net.HttpURLConnection;
          |import java.net.URL;
          |
          |public class HttpRequestExample {
          |    public static void main(String[] args) {
          |        try {
          |            // Specify the URL to send the request to
          |            URL url = new URL("https://jsonplaceholder.typicode.com/posts/1");
          |
          |            // Open a connection to the URL
          |            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
          |
          |            // Set request method to GET
          |            connection.setRequestMethod("GET");
          |
          |            // Get the response code
          |            int responseCode = connection.getResponseCode();
          |            }
          |      }
          |}
          |""".stripMargin,
            "HttpRequestExample.java"
          )
        )
      )

      val ruleCache = RuleCache()
      ruleCache.withRule(
        ConfigAndRules(systemConfig =
          List(
            SystemConfig(
              Constants.apiMethodFullNames,
              "java.net.HttpURLConnection.*",
              Language.UNKNOWN,
              "",
              Array[String]()
            )
          )
        )
      )
      JavaAPISinkTagger.applyTagger(cpg, ruleCache = ruleCache, privadoInput = PrivadoInput(), appCache = AppCache())

      val apiSinks = cpg.call("getResponseCode").l

      apiSinks.tag.nameExact(InternalTag.API_SINK_MARKED.toString).size shouldBe 1
    }
  }

}
