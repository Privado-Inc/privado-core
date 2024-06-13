package ai.privado.languageEngine.go

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.GoFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class GoBasicTest extends GoFrontendTestSuite {
  "Dummy test to write test case" should {
    val ruleCache = RuleCache().setRule(RuleInfoTestData.rule)
    val cpg = code("""
        |package main
        |
        |type User struct {
        |	fn  string
        |	ln  string
        |	eid string
        |	pwd string
        |}
        |
        |func newUser(firstName, lastName, emailId, password string) User {
        |	return User{
        |		fn:  firstName,
        |		ln:  lastName,
        |		eid: emailId,
        |		pwd: password,
        |	}
        |}
        |
        |func main() {
        |}
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
      println(outputJson(Constants.processing).asArray.get)
      // firstName, lastName, emailID, password
      processing.size shouldBe 4
    }
  }
}
