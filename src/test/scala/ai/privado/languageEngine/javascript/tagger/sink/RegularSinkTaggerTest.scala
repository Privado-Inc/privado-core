package ai.privado.languageEngine.javascript.tagger.sink

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.javascript.tagger.sink.RegularSinkTagger
import ai.privado.model.*
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RegularSinkTaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "Sink tagging with cpg.code" should {
    "tag the call node as sink by matching rule with cpg.code" in {
      val cpg = code("""
           |import ecomCart from '@ecomplus/shopping-cart'
           |
           |export default ttq => {
           |  const item = { email : "someemail@email.com"}
           |  ecomCart.on('addItem', ({ item }) => {
           |    ttq.track('AddToCart', item)
           |  })
           |}
           |""".stripMargin)
      cpg.tag.nameExact(Constants.id).value.headOption shouldBe Some("ThirdParties.SDK.Tiktok")
    }

  }

  "Cookie name" should {
    "be tag properly for block node argument" in {
      val cpg = code("""
          |const langPreferenceCookie = cookies[CookieName.LANG_PREFERENCE];
          |if (isUserSpanish(transformedBrowserLocale) || chromeFlag) {
          |  if (langPreferenceCookie) {
          |    userPreferredLocale = langPreferenceCookie;
          |  } else {
          |    setCookie({ /* <=== @libs/common/utils/cookie */
          |      ctx: context,
          |      maxAge: ONE_YEAR_IN_SECONDS,
          |      name: CookieName.LANG_PREFERENCE,
          |      value: userPreferredLocale,
          |    });
          |  }
          |}
          |""".stripMargin)

      cpg.tag.nameExact(Constants.id).value.headOption shouldBe Some(
        Constants.cookieWriteRuleId + ".CookieName.LANG_PREFERENCE"
      )
    }
  }
}

val sinkRule = List(
  RuleInfo(
    Constants.cookieWriteRuleId,
    "Web Storage Cookie(Write)",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List("(?i)(.*setcookie.*)"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SINKS,
    catLevelTwo = Constants.storages,
    Language.JAVASCRIPT,
    Array()
  ),
  RuleInfo(
    "ThirdParties.SDK.Tiktok",
    "Tiktok pixel tracker",
    "",
    FilterProperty.CODE,
    Array(),
    List("ttq.track.*"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SINKS,
    catLevelTwo = Constants.third_parties,
    Language.JAVASCRIPT,
    Array()
  )
)
def code(code: String): Cpg = {
  val inputDir = File.newTemporaryDirectory()
  (inputDir / "sample.js").write(code)
  val outputFile = File.newTemporaryFile()
  val rule: ConfigAndRules =
    ConfigAndRules(List(), sinkRule, List(), List(), List(), List(), List(), List(), List(), List())
  val ruleCache = new RuleCache()
  ruleCache.setRule(rule)
  val config = Config().withInputPath(inputDir.toString()).withOutputPath(outputFile.toString())
  val cpg    = new JsSrc2Cpg().createCpgWithAllOverlays(config).get
  new RegularSinkTagger(cpg, ruleCache).createAndApply()
  cpg
}
