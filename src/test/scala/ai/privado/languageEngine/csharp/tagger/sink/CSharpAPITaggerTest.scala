package ai.privado.languageEngine.csharp.tagger.sink

import ai.privado.languageEngine.csharp.CSharpTestBase
import ai.privado.model.{CatLevelOne, Constants, SourceCodeModel}
import io.shiftleft.semanticcpg.language.*

class CSharpAPITaggerTest extends CSharpTestBase {

  "API call made using HTTP library" should {
    "be tagged as part of API tagger" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
            """
              |using System;
              |using System.Net.Http;
              |using System.Threading.Tasks;
              |
              |class Program
              |{
              |    private static readonly HttpClient client = new HttpClient();
              |
              |    static async Task Main()
              |    {
              |            var user = new Dictionary<string, string>
              |            {
              |               { "email", "hello@example.com" },
              |               { "car", "Mini" }
              |            };
              |            var content = new FormUrlEncodedContent(user);
              |            var response = await client.PostAsync("http://www.example.com", content);
              |            var responseString = await response.Content.ReadAsStringAsync();
              |    }
              |}
              |""".stripMargin,
            "Test.cs"
          )
        )
      )

      val List(postCall) = cpg.call.nameExact("PostAsync").l
      postCall.tag.size shouldBe 6
      postCall.tag.nameExact("id").value.head shouldBe "Sinks.ThirdParties.API.example.com"
      postCall.tag.nameExact("nodeType").value.head shouldBe "api"
      postCall.tag.nameExact("catLevelOne").value.head shouldBe "sinks"
      postCall.tag.nameExact("catLevelTwo").value.head shouldBe "third_parties"
      postCall.tag.nameExact("third_partiesapi").value.head shouldBe "Sinks.ThirdParties.API.example.com"
    }
  }
}
