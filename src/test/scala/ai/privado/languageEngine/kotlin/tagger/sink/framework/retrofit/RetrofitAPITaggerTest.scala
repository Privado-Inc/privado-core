package ai.privado.languageEngine.kotlin.tagger.sink.framework.retrofit

import ai.privado.testfixtures.KotlinFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import ai.privado.cache.RuleCache
import ai.privado.model.*
import ai.privado.rule.{CollectionRuleTestData, RuleInfoTestData}
import ai.privado.tagger.sink.api.APIValidator
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, Constants, SourceCodeModel}

class RetrofitAPITaggerTest extends KotlinFrontendTestSuite {

  val ruleCache =
    RuleCache().setRule(RuleInfoTestData.rule.copy(collections = CollectionRuleTestData.kotlinCollectionRule))

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
      apiMethod.tag.nameExact(Constants.catLevelOne).value.l should not equal List(CatLevelOne.COLLECTIONS.name)
    }

  }
}
