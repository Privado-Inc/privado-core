package ai.privado.languageEngine.kotlin.tagger.collection

import ai.privado.cache.{AppCache, TaggerCache}
import ai.privado.exporter.CollectionExporter
import ai.privado.languageEngine.java.{AbstractTaggingSpec, TestCodeSnippet}
import ai.privado.languageEngine.java.tagger.collection.MethodFullNameCollectionTagger
import ai.privado.languageEngine.java.tagger.source.*
import ai.privado.model.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class KotlinCollectionTaggerTest extends AbstractTaggingSpec {
  val collectionRule = List(
    RuleInfo(
      "Collections.Kotlin.HttpFramework",
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

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i)(.*firstName.*)"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      catLevelTwo = Constants.default,
      Language.KOTLIN,
      Array()
    )
  )

  "Http Collection Endpoints" should {
    "tag get Ktor collection endpoint with method handler as parameter" in {
      var cpg: Cpg = null
      try {
        val fileContents =
          """
          |import io.ktor.application.call
          |import io.ktor.response.respondText
          |import io.ktor.routing.get
          |import io.ktor.routing.routing
          |
          |fun main() {
          |    val firstName = "some first name"
          |    embeddedServer(Netty, 8080) {
          |        routing {
          |            get("/hello") {
          |                call.respondText(firstName)
          |            }
          |        }
          |    }.start(wait = true)
          |}
          |""".stripMargin
        cpg = buildCpg(TestCodeSnippet(sourceCode = fileContents, language = Language.KOTLIN))

        val ruleCache   = ruleCacheWithSourceAndCollectionRules(sourceRule, collectionRule)
        val taggerCache = new TaggerCache()
        val nodeCache   = CPGNodeCacheForSourceTagger(cpg, ruleCache)
        new DirectNodeSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
        new FirstLevelDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
        new OCDDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
        new ExtendingDerivedSourceTagger(cpg, nodeCache, ruleCache, taggerCache).createAndApply()
        val collectionTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
        collectionTagger.createAndApply()

        val ingressRules = collectionTagger.getIngressUrls()
        ingressRules should contain("\"/hello\"")

        val getCalls = cpg.call.methodFullName(".*get.*").l
        getCalls should have size 1
        getCalls.name.toSeq should contain theSameElementsAs List("get")

        val tags = getCalls.head.argument.isMethodRef.head.referencedMethod.tag.l
        tags should have size 6
        tags.nameExact(Constants.id).head.value shouldBe ("Collections.Kotlin.HttpFramework")
        tags.nameExact(Constants.catLevelOne).head.value shouldBe Constants.collections
        tags.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.default
        tags.nameExact(Constants.nodeType).head.value shouldBe "REGULAR"
        tags.nameExact(InternalTag.COLLECTION_METHOD_ENDPOINT.toString).head.value shouldBe "\"/hello\""

        // assert collection exporter
        val collectionExporter   = new CollectionExporter(cpg, ruleCache, appCache = new AppCache())
        val collectionModel :: _ = collectionExporter.getCollections.l
        collectionModel.name should be("Spark Java Http Framework Endpoints")
        collectionModel.collectionId should be("Collections.Kotlin.HttpFramework")
        val collectionOcc :: _ = collectionModel.collections.l
        collectionOcc.sourceId should be("Data.Sensitive.FirstName")
        val collectionOccModel :: _ = collectionOcc.occurrences.l
        collectionOccModel.endPoint should be("\"/hello\"")
      } finally {
        if (cpg != null) {
          cpg.close()
        }
      }
    }
    "tag get Spark Java collection endpoint with method handler as parameter" in {
      var cpg: Cpg = null
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
        cpg = buildCpg(TestCodeSnippet(sourceCode = fileContents, language = Language.KOTLIN))

        val ruleCache        = ruleCacheWithSourceAndCollectionRules(List[RuleInfo](), collectionRule)
        val collectionTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
        collectionTagger.createAndApply()

        val ingressRules = collectionTagger.getIngressUrls()
        ingressRules should contain("\"/hello\"")

        val getCalls = cpg.call.methodFullName(".*get.*").l
        getCalls should have size 1
        getCalls.name.toSeq should contain theSameElementsAs List("get")

        val tags = getCalls.head.argument.isMethodRef.head.referencedMethod.tag.l
        tags should have size 6
        tags.nameExact(Constants.id).head.value shouldBe ("Collections.Kotlin.HttpFramework")
        tags.nameExact(Constants.catLevelOne).head.value shouldBe Constants.collections
        tags.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.default
        tags.nameExact(Constants.nodeType).head.value shouldBe "REGULAR"
        tags.nameExact(InternalTag.COLLECTION_METHOD_ENDPOINT.toString).head.value shouldBe "\"/hello\""
      } finally {
        if (cpg != null) {
          cpg.close()
        }
      }
    }
    "tag get Spark Java collection endpoint with companion method from another class as handler" in {
      var cpg: Cpg = null
      try {
        val fileContents =
          """
          |import spark.Request
          |import spark.Response
          |import spark.Spark.get
          |
          |class HandlerClass {
          |    companion object {
          |        val endpointHandler = { req: Request, res: Response ->
          |            "hello from companion handler"
          |        }
          |    }
          |}
          |
          |fun main() {
          |    get("/hello", HandlerClass.endpointHandler)
          |}
          |""".stripMargin
        cpg = buildCpg(TestCodeSnippet(sourceCode = fileContents, language = Language.KOTLIN))

        val ruleCache        = ruleCacheWithSourceAndCollectionRules(List[RuleInfo](), collectionRule)
        val collectionTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
        collectionTagger.createAndApply()

        val ingressRules = collectionTagger.getIngressUrls()
        ingressRules should contain("\"/hello\"")

        val getCalls = cpg.call.methodFullName(".*get.*").l
        getCalls should have size 1
        getCalls.name.toSeq should contain theSameElementsAs List("get")

        // find the handler, which is an assignment
        val referencedMethod = cpg.call
          .name("<operator>.assignment")
          .where(_.argument(1).code(".*endpointHandler.*"))
          .argument(2)
          .isMethodRef
          .referencedMethod
          .head
        val tags = referencedMethod.tag.l
        tags should have size 6
        tags.nameExact(Constants.id).head.value shouldBe ("Collections.Kotlin.HttpFramework")
        tags.nameExact(Constants.catLevelOne).head.value shouldBe Constants.collections
        tags.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.default
        tags.nameExact(Constants.nodeType).head.value shouldBe "REGULAR"
        tags.nameExact(InternalTag.COLLECTION_METHOD_ENDPOINT.toString).head.value shouldBe "\"/hello\""
      } finally {
        if (cpg != null) {
          cpg.close()
        }
      }
    }
    "tag get Spark Java collection endpoint with method from an object class as handler" in {
      var cpg: Cpg = null
      try {
        val fileContents =
          """
          |import spark.Request
          |import spark.Response
          |import spark.Spark.get
          |
          |object AnotherHandlerClass {
          |    fun endpointHandler(req: Request, res: Response): String {
          |        return "hello"
          |    }
          |}
          |
          |fun main() {
          |    get("/hello", AnotherHandlerClass::endpointHandler)
          |}
          |""".stripMargin
        cpg = buildCpg(TestCodeSnippet(sourceCode = fileContents, language = Language.KOTLIN))

        val ruleCache        = ruleCacheWithSourceAndCollectionRules(List[RuleInfo](), collectionRule)
        val collectionTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
        collectionTagger.createAndApply()

        val ingressRules = collectionTagger.getIngressUrls()
        ingressRules should contain("\"/hello\"")

        val getCalls = cpg.call.methodFullName(".*get.*").l
        getCalls should have size 1
        getCalls.name.toSeq should contain theSameElementsAs List("get")

        val tags = cpg.method.fullName(".*endpointHandler.*").head.tag.l
        tags should have size 6
        tags.nameExact(Constants.id).head.value shouldBe ("Collections.Kotlin.HttpFramework")
        tags.nameExact(Constants.catLevelOne).head.value shouldBe Constants.collections
        tags.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.default
        tags.nameExact(Constants.nodeType).head.value shouldBe "REGULAR"
        tags.nameExact(InternalTag.COLLECTION_METHOD_ENDPOINT.toString).head.value shouldBe "\"/hello\""
      } finally {
        if (cpg != null) {
          cpg.close()
        }
      }
    }
  }

}
