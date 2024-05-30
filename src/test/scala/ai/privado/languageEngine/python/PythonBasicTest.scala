package ai.privado.languageEngine.python

import ai.privado.testfixtures.PythonFrontendTestSuite
import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.semanticcpg.language.*

class PythonBasicTest extends PythonFrontendTestSuite {

  "Dummy test to write test case" should {
    val ruleCache = RuleCache().setRule(RuleInfoTestData.rule)
    val cpg = code("""
                     |class User:
                     |    firstName: str
                     |    passwd: str
                     |    emailId: str
                     |
                     |def main():
                     |    # Create an instance of the User data class
                     |    user = User(
                     |        firstName="firstName1",
                     |        passwd="yourPassword",
                     |        emailId="yourEmail@example.com"
                     |    )
                     |
                     |    # Access and print the properties
                     |    print(user)
                     |
                     |if __name__ == "__main__":
                     |    main()
                     |
                     |""".stripMargin)
      .withRuleCache(ruleCache)

    "show sources matches using cpg" in {
      val emailIdIdentifier = cpg.identifier("emailId")
      emailIdIdentifier.nonEmpty shouldBe true
      emailIdIdentifier.tag.nameExact(Constants.id).value.head shouldBe "Data.Sensitive.ContactData.EmailAddress"

      val userNameIdentifier = cpg.identifier("firstName")
      userNameIdentifier.nonEmpty shouldBe true
      userNameIdentifier.tag.nameExact(Constants.id).value.head shouldBe "Data.Sensitive.FirstName"
    }

    "show using output json" in {
      val outputJson = cpg.getPrivadoJson()
      outputJson(Constants.repoName).asString.get should startWith("x2cpgTestTmpDir")

      val processing = outputJson(Constants.processing).asArray.get
      processing.size shouldBe 2
    }
  }
}
