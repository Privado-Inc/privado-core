package ai.privado.languageEngine.javascript

import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, SystemConfig}
import ai.privado.rule.{DEDRuleTestData, RuleInfoTestData}
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.semanticcpg.language.*

class JavascriptBasicTest extends JavaScriptFrontendTestSuite {

  "Check normal source tagging flow" should {
    val ruleCache = RuleCache().setRule(RuleInfoTestData.rule)
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

  "Check support for ai_inference rules" should {
    val ruleCache = RuleCache().setRule(
      RuleInfoTestData.rule
        .copy(dedRules = List(DEDRuleTestData.dedRuleTestJS))
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

    "should match the regular variable not present in ai_inference" in {
      val userNameIdentifier = cpg.identifier("userName")
      userNameIdentifier.nonEmpty shouldBe true
      userNameIdentifier.tag.nameExact(Constants.id).value.head shouldBe "Data.Sensitive.User"
    }

    "should match the variable tagged by ai_inference as PII" in {
      val passwordIdentifier = cpg.identifier("passwd").l
      val tags               = passwordIdentifier.tag.l
      passwordIdentifier.nonEmpty shouldBe true

      val idTags = tags.nameExact(Constants.id).value
      idTags.nonEmpty shouldBe true
      idTags.head shouldBe "Data.Sensitive.AccountData.AccountPassword"

      val taggedbyDED = tags.filter(t => t.name.contains(InternalTag.TAGGED_BY_DED.toString)).l
      taggedbyDED.size shouldBe 1
    }

    "should not tag the variable disabled by ai_inference for PII tagging" in {
      val emailIdIdentifier = cpg.identifier("emailId").l
      val tags              = emailIdIdentifier.tag.l
      emailIdIdentifier.nonEmpty shouldBe true

      val taggedbyDEDDisabled = tags.filter(t => t.name.contains(InternalTag.TAGGING_DISABLED_BY_DED.toString)).l
      taggedbyDEDDisabled.size shouldBe 1
    }

    "show using output json" in {
      val outputJson = cpg.getPrivadoJson()
      outputJson(Constants.repoName).asString.get should startWith("x2cpgTestTmpDir")

      val processing = outputJson(Constants.processing).asArray.get
      processing.size shouldBe 2
    }
  }
}
