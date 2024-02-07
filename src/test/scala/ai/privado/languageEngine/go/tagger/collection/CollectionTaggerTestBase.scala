package ai.privado.languageEngine.go.tagger.collection

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{CatLevelOne, ConfigAndRules, FilterProperty, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class CollectionTaggerTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

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

    ruleCache.setRule(rule)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputDir.delete()
    super.afterAll()
  }

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*firstName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.GO,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, List(), List(), List(), List(), List(), List(), List(), List(), List())
  val taggerCache = new TaggerCache()
}
