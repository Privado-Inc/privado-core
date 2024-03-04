package ai.privado.languageEngine.csharp.dataflow

import ai.privado.languageEngine.csharp.CSharpTestBase
import ai.privado.model.SourceCodeModel
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
class DataflowTests extends CSharpTestBase {
  implicit val engineContext: EngineContext = new EngineContext()
  "simple dataflows" should {
    "find a path from source to sink through a single step" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
            """
          |namespace Foo {
          | public class Bar {
          |   public static void Main() {
          |     int phoneNumber = 123;
          |     Console.WriteLine(phoneNumber);
          |   }
          | }
          |}
          |""".stripMargin,
            "Test.cs"
          )
        )
      )

      val src  = cpg.identifier("phoneNumber").lineNumber(5).l
      val sink = cpg.call.nameExact("WriteLine").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }
}
