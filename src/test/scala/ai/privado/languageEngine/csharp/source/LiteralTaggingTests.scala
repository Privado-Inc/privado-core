package ai.privado.languageEngine.csharp.source

import ai.privado.languageEngine.csharp.CSharpTestBase
import ai.privado.model.{CatLevelOne, Constants, SourceCodeModel}
import io.shiftleft.semanticcpg.language.*

class LiteralTaggingTests extends CSharpTestBase {

  "Literals in code" should {
    "be tagged as part of Literal tagger" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
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
          )
        )
      )

      val List(phoneNumber) = cpg.literal.l
      phoneNumber.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.SOURCES.name)
    }
  }

}
