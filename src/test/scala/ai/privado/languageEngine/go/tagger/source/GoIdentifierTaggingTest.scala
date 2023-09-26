package ai.privado.languageEngine.go.tagger.source

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.model.*
import io.shiftleft.semanticcpg.language.*

class GoIdentifierTaggingTest extends GoTaggingTestBase {


  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
  }

  override val goFileContents =
    """
      package main
      |
      |type User struct {
      |	FirstName     string
      |	Age      int
      |	Location string
      |	Email    string
      |}
      |
      |func main() {
      |	// Creating a user instance
      |	user := User{
      |		Name:     "John Doe",
      |		Age:      25,
      |		Location: "New York",
      |		Email: "abc@gmail.com",
      |	}
      |}
      |
      |""".stripMargin

  "Tagging derived sources" should {
    "tag member in a structure" in {
      val identifierNodes = cpg.member("FirstName").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"
    }

    "tag user(of type structure) object" in {
      val List(userIdentifier) = cpg.identifier("user").lineNumber(13).l
      userIdentifier.tag.where(_.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString)).value.head shouldBe "Data.Sensitive.FirstName"
      userIdentifier.tag.where(_.nameExact(Constants.id)).size shouldBe 1
      userIdentifier.tag.where(_.nameExact(Constants.catLevelOne)).value.head shouldBe "DerivedSources"
    }

  }

}
