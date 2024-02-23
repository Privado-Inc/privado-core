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

}
