package ai.privado.languageEngine.go.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.go.passes.SQLQueryParser
import ai.privado.model.*
import ai.privado.passes.SQLParser
import ai.privado.tagger.source.SqlQueryTagger
import better.files.File
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.compiletime.uninitialized

abstract class DataElementDiscoveryTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = uninitialized
  val goFileContentMap: Map[String, String]
  var inputDir: File  = uninitialized
  var outputDir: File = uninitialized
  val ruleCache       = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- goFileContentMap) {
      (inputDir / s"$key.go").write(content)
    }

    outputDir = File.newTemporaryDirectory()

    val config = Config(fetchDependencies = true)
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputDir.pathAsString)
    val goSrc2Cpg = new GoSrc2Cpg(None)
    val xtocpg = goSrc2Cpg.createCpg(config).map { cpg =>
      new SQLQueryParser(cpg).createAndApply()
      new SQLParser(cpg, inputDir.pathAsString, ruleCache).createAndApply()
      new SqlQueryTagger(cpg, ruleCache).createAndApply()
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

  val sourceRule: List[RuleInfo] = List(
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
