package ai.privado.languageEngine.csharp.source

import ai.privado.languageEngine.csharp.CSharpTestBase
import ai.privado.model.*
import io.shiftleft.semanticcpg.language.*

class IdentifierTaggingTests extends CSharpTestBase {

  "Basic assignment nodes" should {
    "be tagged as part of identifier tagger" in {
      val (cpg, _) = code("""
          |namespace Foo {
          | public class Bar {
          |   public static void Main(string[] args) {
          |     var phoneNumber = "1234_5678";
          |   }
          | }
          |}
          |""".stripMargin)

      val List(phoneNumber) = cpg.identifier.nameExact("phoneNumber").l
      phoneNumber.tag.nameExact(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString).size shouldBe 1
    }
  }

  "Derived sources" should {
    val (cpg, _) = code("""
        |namespace Foo {
        | public class Bar {
        |   public int PhoneNumber {get; set;}
        | }
        | public class Baz {
        |   public static void Main() {
        |     Bar b = new Bar(1);
        |   }
        | }
        |}
        |""".stripMargin)

    "tag the member inside a class" in {
      cpg.member("PhoneNumber").tag.nameExact(Constants.id).value.l shouldBe List(
        "Data.Sensitive.ContactData.PhoneNumber"
      )
    }

    "be tagged as part of Identifier tagger" in {
      val barId = cpg.identifier("b").l
      barId.tag
        .where(_.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString))
        .value
        .head shouldBe "Data.Sensitive.ContactData.PhoneNumber"
      barId.tag.where(_.nameExact(Constants.id)).size shouldBe 1
      barId.tag.where(_.nameExact(Constants.catLevelOne)).value.head shouldBe "DerivedSources"
    }
  }

}
