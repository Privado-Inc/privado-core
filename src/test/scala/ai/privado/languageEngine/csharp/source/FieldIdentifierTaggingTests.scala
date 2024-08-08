package ai.privado.languageEngine.csharp.source

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, ConfigAndRules, SourceCodeModel}
import ai.privado.testfixtures.CSharpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import ai.privado.rule.RuleInfoTestData

class FieldIdentifierTaggingTests extends CSharpFrontendTestSuite {

  val configAnndRules: ConfigAndRules =
    ConfigAndRules(sources = RuleInfoTestData.sourceRule)

  val ruleCache = new RuleCache().setRule(configAnndRules)

  "field access in code" ignore {
    val cpg = code(
      """
            |namespace Foo {
            | public class Bar {
            |   val firstName = "Alice";
            |   public static void Main() {
            |     this.firstName = "Bob";
            |   }
            | }
            |}
            |""".stripMargin,
      "Test.cs"
    ).withRuleCache(ruleCache)

    "be tagged as part of identifier tagger" in {

      val List(firstNameField) = cpg.fieldAccess.l
      firstNameField.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.SOURCES.name)
    }
  }
}
