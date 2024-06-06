package ai.privado.languageEngine.javascript

import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.semanticcpg.language.*

class JavascriptBasicTest extends JavaScriptFrontendTestSuite {

  "Dummy test to write test case" should {
    val ruleCache = RuleCache().setRule(
      RuleInfoTestData.rule
        .copy(sources = RuleInfoTestData.rule.sources, sinks = RuleInfoTestData.rule.sinks)
    )
    val cpg = code("""
                     |import { HttpClient } from '@angular/common/http';
                     |
                     |@Injectable({
                     |  providedIn: 'root',
                     |})
                     |export class SharedService {
                     |  constructor(private http: HttpClient) {}
                     |
                     |  saveUserFeedback(payload) {
                     |    return this.http.post(
                     |      'albert-the-cat@jobcloud.ch',
                     |      payload
                     |    );
                     |  }
                     |}
                     |
                     |
                     |export class Auth {
                     |  login(emailId, passwd, userName = "guest-user") {
                     |    const accountId = "jhgejkrbg";
                     |    console.log(emailId, passwd, userName);
                     |  }
                     |}
                     |""".stripMargin)
      .withRuleCache(ruleCache)

    "show sources matches using cpg" in {
      val emailIdIdentifier = cpg.identifier("emailId")
      emailIdIdentifier.nonEmpty shouldBe true
      emailIdIdentifier.tag.nameExact(Constants.id).value.head shouldBe "Data.Sensitive.ContactData.EmailAddress"

      val userNameIdentifier = cpg.identifier("userName")
      userNameIdentifier.nonEmpty shouldBe true
      userNameIdentifier.tag.nameExact(Constants.id).value.head shouldBe "Data.Sensitive.User"
    }

    "show using output json" in {
      val outputJson = cpg.getPrivadoJson()
      outputJson(Constants.repoName).asString.get should startWith("x2cpgTestTmpDir")

      val processing = outputJson(Constants.processing).asArray.get
      processing.size shouldBe 2
    }
  }
}
