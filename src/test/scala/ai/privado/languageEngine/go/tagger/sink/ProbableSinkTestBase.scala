package ai.privado.languageEngine.go.tagger.sink

import ai.privado.cache.{AppCache, RuleCache, TaggerCache}
import ai.privado.model.{CatLevelOne, ConfigAndRules, FilterProperty, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class ProbableSinkTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val goFileContentMap: Map[String, String]
  var inputDir: File  = _
  var outputDir: File = _
  val ruleCache       = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- goFileContentMap) {
      (inputDir / s"$key.go").write(content)
    }
    outputDir = File.newTemporaryDirectory()

    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputDir.pathAsString)
    val goSrc  = new GoSrc2Cpg()
    val xtocpg = goSrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

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

  val sinkRule: List[RuleInfo] = List(
    RuleInfo(
      "Leakages.Log.Fatal",
      "Log Error",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*(fatal).*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.GO,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(List(), sinkRule, List(), List(), List(), List(), List(), List(), List(), List())
  val taggerCache = new TaggerCache()
}
