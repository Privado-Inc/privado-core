package ai.privado.languageEngine.java.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{CatLevelOne, ConfigAndRules, FilterProperty, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class DataElementDiscoveryTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val javaFileContentMap: Map[String, String]
  var inputDir: File  = _
  var outputDir: File = _
  val ruleCache       = new RuleCache()
  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- javaFileContentMap) {
      (inputDir / s"$key.java").write(content)
    }
    outputDir = File.newTemporaryDirectory()

    val config =
      Config(fetchDependencies = true, delombokMode = Some("no-delombok"))
        .withInputPath(inputDir.pathAsString)
        .withOutputPath(outputDir.pathAsString)
    val javaSrc = new JavaSrc2Cpg()
    val xtocpg = javaSrc.createCpg(config).map { cpg =>
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
      Language.JAVA,
      Array()
    )
  )

  val collectionRule = List(
    RuleInfo(
      "Collections.Annotation.Spring",
      "Spring Web Interface Annotation",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("RequestMapping|PostMapping|PutMapping|GetMapping|DeleteMapping"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = "annotations",
      Language.JAVA,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, List(), collectionRule, List(), List(), List(), List(), List(), List(), List())

  val taggerCache = new TaggerCache()

}
