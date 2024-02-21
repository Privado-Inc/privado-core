package ai.privado.languageEngine.java.tagger.collection

import ai.privado.languageEngine.java.JavaTaggingTestBase
import ai.privado.model.Constants
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
      val tags = callNode.argument.isMethodRef.head.referencedMethod.tag.l
      tags.size shouldBe 6
      tags.nameExact(Constants.id).head.value shouldBe ("Collections.Spark.HttpFramework")
      tags.nameExact(Constants.catLevelOne).head.value shouldBe Constants.collections
      tags.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.default
      tags.nameExact(Constants.nodeType).head.value shouldBe "REGULAR"
      tags.nameExact("COLLECTION_METHOD_ENDPOINT").head.value shouldBe "\"/hello\""
    }
  }
}
