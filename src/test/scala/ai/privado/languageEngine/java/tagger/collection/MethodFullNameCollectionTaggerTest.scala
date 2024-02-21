package ai.privado.languageEngine.java.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.AbstractTaggingSpec
import ai.privado.model.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class MethodFullNameCollectionTaggerTest extends AbstractTaggingSpec(language = Language.JAVA) {
  val collectionRule: RuleInfo = RuleInfo(
    "Collections.Spark.HttpFramework",
    "Spark Java Http Framework Endpoints",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List(".*\\b(get|post|put)\\b.*"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.COLLECTIONS,
    catLevelTwo = Constants.default,
    Language.JAVA,
    Array()
  )

  "Simple Spark Http sample" should {
    "should tag get collection endpoint" in {
      var cpg: Option[Cpg] = Option.empty[Cpg]
      try {
        val javaFileContents: String =
          """
            |import static spark.*;
            |
            |public class HelloWorld {
            |    public static void main(String[] args) {
            |        Spark.get("/hello", (req, res) -> "Hello World");
            |    }
            |}""".stripMargin
        cpg = Some(buildCpg(javaFileContents))
        cpg.get.call.methodFullName(".*get.*").l.size shouldBe 1
        cpg.get.call.head.code.contains("get") shouldBe true

        val collectionTagger = new MethodFullNameCollectionTagger(cpg.get, ruleCacheWithCollectionRule(collectionRule))
        collectionTagger.createAndApply()

        val ingressRules = collectionTagger.getIngressUrls()
        ingressRules should contain("\"/hello\"")

        val callNode = cpg.get.call.methodFullName(".*get.*").head
        callNode.name shouldBe "get"
        val tags = callNode.argument.isMethodRef.head.referencedMethod.tag.l
        tags.size shouldBe 6
        tags.nameExact(Constants.id).head.value shouldBe ("Collections.Spark.HttpFramework")
        tags.nameExact(Constants.catLevelOne).head.value shouldBe Constants.collections
        tags.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.default
        tags.nameExact(Constants.nodeType).head.value shouldBe "REGULAR"
        tags.nameExact("COLLECTION_METHOD_ENDPOINT").head.value shouldBe "\"/hello\""
      } finally {
        if (cpg.isDefined) {
          cpg.get.close()
        }
      }
    }
  }
}
