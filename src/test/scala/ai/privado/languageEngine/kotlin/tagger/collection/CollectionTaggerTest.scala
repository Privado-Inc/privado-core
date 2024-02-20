package ai.privado.languageEngine.kotlin.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.CollectionTagger
import ai.privado.model.*
import io.shiftleft.semanticcpg.language.*

class CollectionTaggerTest extends KotlinTaggingTestBase {
  private val collectionRule = List(
    RuleInfo(
      "Collections.Spark.HttpFramework",
      "Spark Java Http Framework Endpoints",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(".*(.get).*"),
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
        |fun main(args: Array<String>) {
        |    Spark.get("/hello") { req, res ->
        |        "Hello Spark Kotlin!"
        |    }
        |}
        |""".stripMargin
    initCpg(fileContents)
    "should tag get endpoint" in {
      cpg.call.methodFullName(".*get.*").l.size shouldBe 1
      cpg.call.head.code.contains("Spark") shouldBe true

      val collectionTagger = new KotlinCollectionTagger(cpg, ruleCache)
      collectionTagger.createAndApply()

      val ingressRules = collectionTagger.getIngressUrls()
      ingressRules contains "/hello1"
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
