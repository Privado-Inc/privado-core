package ai.privado.languageEngine.kotlin.tagger.collection

import ai.privado.languageEngine.java.AbstractTaggingSpec
import ai.privado.languageEngine.java.tagger.collection.MethodFullNameCollectionTagger
import ai.privado.model.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class KotlinCollectionTaggerTest extends AbstractTaggingSpec(language = Language.KOTLIN) {
  val collectionRule: RuleInfo = RuleInfo(
    "Collections.Spark.HttpFramework",
    "Spark Java Http Framework Endpoints",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List(".*.get.*"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.COLLECTIONS,
    catLevelTwo = Constants.default,
    Language.KOTLIN,
    Array()
  )

  "Spark Http Collection Endpoints" should {
    "tag get collection endpoint with method handler as parameter" in {
      var cpg: Option[Cpg] = Option.empty[Cpg]
      try {
        val fileContents =
          """
          |import spark.Spark
          |
          |fun main(args: Array<String>) {
          |    Spark.get("/hello") { req, res ->
          |        "Hello Spark Kotlin!"
          |    }
          |}
          |""".stripMargin
        cpg = Some(buildCpg(fileContents))
        cpg.get.call.methodFullName(".*.get.*").l.size shouldBe 1
        cpg.get.call.head.code.contains("Spark.get") shouldBe true

        val collectionTagger = new MethodFullNameCollectionTagger(cpg.get, ruleCacheWithCollectionRule(collectionRule))
        collectionTagger.createAndApply()

        val ingressRules = collectionTagger.getIngressUrls()
        ingressRules should contain("\"/hello\"")

        val callNode = cpg.get.call.methodFullName(".*.get.*").head
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
