package ai.privado.languageEngine.javascript.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{ConfigAndRules, Language, SystemConfig}
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class APIReportTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

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

  val systemConfig = List(
    SystemConfig(
      "apiHttpLibraries",
      "(?i)(request|fetch|axios|vue-axios|urllib|http|client|react-query|swr|socket(.){0,1}io|xmlhttprequest|node.http|cors|got|apollo|superagent|wretch|@angular\\/common\\/http|@(.){2,25}\\/http|.*(HttpClient)|reconnecting-websocket).*",
      Language.JAVASCRIPT,
      "",
      Array()
    ),
    SystemConfig(
      "apiSinks",
      "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|usequery|useSWR|useSWRInfinite|useSWRSubscription|fetch|fetchapi|fetchlegacyxml|createfetch|postform|axios|cors|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity|call|createCall|createEndpoint|dispatch|invoke|newMessage|getInput|getOutput|getResponse|marshall|unmarshall|send|asyncSend|emit|on|track|addEventListener)",
      Language.JAVASCRIPT,
      "",
      Array()
    ),
    SystemConfig(
      "ignoredSinks",
      "(?i).*(?<=map|list|jsonobject|json|array|arrays|jsonnode|objectmapper|objectnode).*(put:|get:).*",
      Language.JAVASCRIPT,
      "",
      Array()
    )
  )

  val taggerCache = new TaggerCache()
  val rule: ConfigAndRules =
    ConfigAndRules(List(), List(), List(), List(), List(), List(), List(), List(), systemConfig, List())
}
