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
