package ai.privado.languageEngine.kotlin.tagger.sink.framework.retrofit

import ai.privado.testfixtures.KotlinFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import ai.privado.cache.RuleCache
import ai.privado.model.*
import ai.privado.rule.RuleInfoTestData
import ai.privado.tagger.sink.api.APIValidator
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, Constants, SourceCodeModel}

class RetrofitAPITaggerTest extends KotlinFrontendTestSuite {

  val collectionRule = List(
    RuleInfo(
      "Collections.Kotlin.Annotation",
      "Annotation",
      "",
      FilterProperty.CODE,
      Array(),
      List(".*(GET|PUT|POST|DELETE|HEAD|OPTIONS).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = Constants.annotations,
      Language.KOTLIN,
      Array()
    )
  )

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.User",
      "User",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i)(.*userName.*)"),
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
  val ruleCache =
    RuleCache().setRule(RuleInfoTestData.rule.copy(collections = collectionRule).copy(sources = sourceRule))

  "API call made using Retrofit library" should {
    val cpg = code(
      """
        |import retrofit2.http.POST
        |
        |interface Username {
        |    @Headers("Content-Type: application/json")
        |    @POST("/username")
        |    fun sendUserName(@Body userName: String): Call<Void>
        |}
        |""".stripMargin,
      "Test.kt"
    ).withRuleCache(ruleCache)

    "have correct annotation" in {
      val List(postAnnotation) = cpg.annotation.name(".*POST.*").l
      postAnnotation.code shouldBe "@POST(\"/username\")"
      postAnnotation.fullName shouldBe "retrofit2.http.POST"
    }

    "not be tagged as a collection" in {
      val apiMethod = cpg.method.name("sendUserName").l
      // TODO: This shouldNotBe. For this fix global collection tagger by filtering on retrofit
      apiMethod.tag.nameExact(Constants.catLevelOne).value.l should not equal List(CatLevelOne.COLLECTIONS.name)
    }

  }
}
