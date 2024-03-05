package ai.privado.languageEngine.csharp.source

import ai.privado.languageEngine.csharp.CSharpTestBase
import ai.privado.model.{CatLevelOne, Constants, SourceCodeModel}
import io.shiftleft.semanticcpg.language.*

class FieldIdentifierTaggingTests extends CSharpTestBase {
  "field access in code" should {
    "be tagged as part of identifier tagger" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
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
          )
        )
      )

      val List(firstNameField) = cpg.fieldAccess.l
      firstNameField.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.SOURCES.name)
    }
  }
}
