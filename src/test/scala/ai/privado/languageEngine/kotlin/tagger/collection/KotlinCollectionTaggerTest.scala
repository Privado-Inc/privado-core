package ai.privado.languageEngine.kotlin.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.MethodFullNameCollectionTagger
import ai.privado.model.*
import io.shiftleft.semanticcpg.language.*

class KotlinCollectionTaggerTest extends KotlinTaggingTestBase {
  private val collectionRule = List(
    RuleInfo(
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
  )

  "Simple Spark Http sample" should {
    val ruleCache: RuleCache = getRuleCache
    val fileContents = """
        |import spark.*;
        |
        |fun defineMappings() {
        |    Spark.get("/hello") { req, res ->
        |        "Hello Spark Kotlin!"
        |    }
        |}
        |
        |fun main(args: Array<String>) {
        |    defineMappings()
        |}
        |""".stripMargin
    initCpg(fileContents)
    "should tag get collection endpoint" in {
      cpg.call.methodFullName(".*.get.*").l.size shouldBe 1
      cpg.call.head.code.contains("Spark.get") shouldBe true

      val collectionTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
      collectionTagger.createAndApply()

      val callNode = cpg.call.methodFullName(".*.get.*").head
      callNode.name shouldBe "get"
      callNode.tag.size shouldBe 2
//      callNode.tag.nameExact(Constants.catLevelOne).head.value shouldBe Constants.collections
//      callNode.tag.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.default

      val ingressRules = collectionTagger.getIngressUrls()
      ingressRules should contain("\"/hello\"")
    }
  }

  override def getRuleCache: RuleCache = {
    val rule: ConfigAndRules =
      ConfigAndRules(List(), List(), collectionRule, List(), List(), List(), List(), List(), List(), List())
    val ruleCache = RuleCache()
    ruleCache.setRule(rule)
    ruleCache
  }
}
