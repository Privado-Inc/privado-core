package ai.privado.languageEngine.csharp.source

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, SourceCodeModel}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.CSharpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class LiteralTaggingTests extends CSharpFrontendTestSuite {

  val configAnndRules: ConfigAndRules =
    ConfigAndRules(sources = RuleInfoTestData.sourceRule)

  val ruleCache = new RuleCache().setRule(configAnndRules)

  "Literals in code" ignore {
    val cpg = code(
      """
            |namespace Foo {
            | public class Bar {
            |   public static void Main() {
            |     addPhone("phone");
            |   }
            |
            |   public static void addPhone(String phoneNumber) {}
            | }
            |}
            |""".stripMargin,
      "Test.cs"
    ).withRuleCache(ruleCache)

    "be tagged as part of Literal tagger" in {

      val List(phoneNumber) = cpg.literal.l
      phoneNumber.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.SOURCES.name)
    }
  }
}
