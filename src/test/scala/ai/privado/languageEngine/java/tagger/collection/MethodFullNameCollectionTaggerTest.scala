package ai.privado.languageEngine.java.tagger.collection

import ai.privado.languageEngine.java.JavaTaggingTestBase
import io.shiftleft.semanticcpg.language.*

class MethodFullNameCollectionTaggerTest extends JavaTaggingTestBase {
  override val javaFileContents: String =
    """
    |import static spark.*;
    |
    |public class HelloWorld {
    |    public static void main(String[] args) {
    |        Spark.get("/hello", (req, res) -> "Hello World");
    |    }
    |}""".stripMargin

  "Simple Spark Http sample" should {
    "should tag get collection endpoint" in {
      cpg.call.methodFullName(".*get.*").l.size shouldBe 1
      cpg.call.head.code.contains("get") shouldBe true

      val collectionTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
      collectionTagger.createAndApply()

      val ingressRules = collectionTagger.getIngressUrls()
      ingressRules should contain("\"/hello\"")

      val callNode = cpg.call.methodFullName(".*get.*").head
      callNode.name shouldBe "get"
      callNode.ast.head.tag.size shouldBe 2
    }
  }
}
