package ai.privado.languageEngine.csharp.source

import ai.privado.cache.RuleCache
import ai.privado.model.*
import ai.privado.testfixtures.CSharpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import ai.privado.rule.RuleInfoTestData
import ai.privado.traversal.TraversalValidator

class IdentifierTaggingTests extends CSharpFrontendTestSuite with TraversalValidator {

  val configAnndRules: ConfigAndRules =
    ConfigAndRules(sources = RuleInfoTestData.sourceRule)

  val ruleCache = new RuleCache().setRule(configAnndRules)

  "Basic assignment nodes" ignore {
    val cpg = code(
      """
          |namespace Foo {
          | public class Bar {
          |   public static void Main(string[] args) {
          |     var phoneNumber = "1234_5678";
          |   }
          | }
          |}
          |""".stripMargin,
      "Test.cs"
    ).withRuleCache(ruleCache)

    "be tagged as part of identifier tagger" in {

      val List(phoneNumber) = cpg.identifier.nameExact("phoneNumber").l
      phoneNumber.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.SOURCES.name)
    }
  }

  "Derived sources" ignore {
    val cpg = code(
      """
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
        |""".stripMargin,
      "Test.cs"
    ).withRuleCache(ruleCache)

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
      barId.tag.where(_.nameExact(Constants.catLevelOne)).value.l shouldBe List(CatLevelOne.DERIVED_SOURCES.name)
    }

    "build correct edges between derived and original sources" in {
      val List(identifierNode)    = cpg.identifier("b").l
      val List(phoneNumberMember) = cpg.member("PhoneNumber").l
      originalSourceTraversalValidator(identifierNode, "Data.Sensitive.ContactData.PhoneNumber")
      derivedSourceTraversalValidator(phoneNumberMember, identifierNode)
    }
  }
}
