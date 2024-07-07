package ai.privado.languageEngine.java.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, SystemConfig}
import ai.privado.rule.{DEDRuleTestData, RuleInfoTestData}
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.semanticcpg.language.*

class DEDTaggerTest extends JavaFrontendTestSuite {
  "Check support for ai_inference rules" should {
    val ruleCache = RuleCache().setRule(
      RuleInfoTestData.rule
        .copy(dedRules = List(DEDRuleTestData.dedRuleTestJava))
    )

    val cpg = code(
      """
        |public class User {
        |   public String userName;
        |   public String passwd;
        |   public String emailId;
        |
        |   public String getUserName() {return userName;}
        |   public void setUserName(String firstName) {this.userName = userName;}
        |}
        |""".stripMargin,
      "User.java"
    )
      .withRuleCache(ruleCache)

    "should match the regular variable not present in ai_inference" in {
      val userNameIdentifier = cpg.member("userName")
      userNameIdentifier.nonEmpty shouldBe true
      userNameIdentifier.tag.nameExact(Constants.id).value.l shouldBe List("Data.Sensitive.User")
    }

    "should match the variable tagged by ai_inference as PII" in {
      val passwordIdentifier = cpg.member("passwd").l
      val tags               = passwordIdentifier.tag.l
      passwordIdentifier.nonEmpty shouldBe true

      val idTags = tags.nameExact(Constants.id).value
      idTags.nonEmpty shouldBe true
      idTags.l shouldBe List("Data.Sensitive.AccountData.AccountPassword")

      val taggedbyDED = tags.filter(t => t.name.contains(InternalTag.TAGGED_BY_DED.toString)).l
      taggedbyDED.size shouldBe 1
    }

    "should not tag the variable disabled by ai_inference for PII tagging" in {
      val emailIdIdentifier = cpg.member("emailId").l
      val tags              = emailIdIdentifier.tag.l
      emailIdIdentifier.nonEmpty shouldBe true

      val taggedbyDEDDisabled = tags.filter(t => t.name.contains(InternalTag.TAGGING_DISABLED_BY_DED.toString)).l
      taggedbyDEDDisabled.size shouldBe 1
    }

  }

}
