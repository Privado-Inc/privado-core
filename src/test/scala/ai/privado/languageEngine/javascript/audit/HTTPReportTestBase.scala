package ai.privado.languageEngine.javascript.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{ConfigAndRules, Language, SystemConfig}
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class HTTPReportTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val javascriptFileContentMap: Map[String, String]
  var inputDir: File  = _
  var outputDir: File = _
  val ruleCache       = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- javascriptFileContentMap) {
      (inputDir / key).write(content)
    }

    outputDir = File.newTemporaryDirectory()

    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputDir.pathAsString)
    val xtocpg = new JsSrc2Cpg().createCpgWithAllOverlays(config)
    cpg = xtocpg.get

    ruleCache.withRule(rule)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputDir.delete()
    super.afterAll()
  }

  val taggerCache = new TaggerCache()
  val rule: ConfigAndRules =
    ConfigAndRules(List(), List(), List(), List(), List(), List(), List(), List(), List(), List())
}
