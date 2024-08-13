package ai.privado.languageEngine.csharp.tagger.sink

import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, Constants, SourceCodeModel}
import ai.privado.testfixtures.CSharpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import ai.privado.cache.RuleCache
import ai.privado.model.*
import ai.privado.rule.RuleInfoTestData
import ai.privado.tagger.sink.api.APIValidator

class CSharpAPITaggerTest extends CSharpFrontendTestSuite with APIValidator {

  val systemConfig = List(
    SystemConfig("apiHttpLibraries", "^(?i)(HttpClient).*", Language.CSHARP, "", Array()),
    SystemConfig(
      "apiSinks",
      "(?i)((Get|Post|Put|Patch|Delete)(String|ByteArray|Stream)?Async)",
      Language.CSHARP,
      "",
      Array()
    ),
    SystemConfig(
      "apiIdentifier",
      "(?i).*((hook|base|auth|prov|endp|install|request|service|gateway|route|resource)(.){0,12}url|(slack|web)(.){0,4}hook|(rest|api|request|service)(.){0,4}(endpoint|gateway|route)).*",
      Language.CSHARP,
      "",
      Array()
    )
  )
  val ruleCache = RuleCache().setRule(RuleInfoTestData.rule.copy(systemConfig = systemConfig))

  "API call made using System.Net.Http library" ignore {
    val cpg = code(
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
    ).withRuleCache(ruleCache)

    "tag sink as api sink" in {
      val List(getUserCall) = cpg.call("PostAsync").l
      assertAPISinkCall(getUserCall)
    }

    "tag sink with endpoint" in {
      val List(getUserCall) = cpg.call("PostAsync").l
      assertAPIEndpointURL(getUserCall, "\"http://www.example.com\"")
    }
  }
}
