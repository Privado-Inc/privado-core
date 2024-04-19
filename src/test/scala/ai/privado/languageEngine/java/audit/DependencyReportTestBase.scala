package ai.privado.languageEngine.java.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, FilterProperty, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class DependencyReportTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val javaFileContentMap: Map[String, String]
  var inputDir: File  = _
  var outputDir: File = _
  val ruleCache       = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "project").createDirectoryIfNotExists()
    for ((key, content) <- javaFileContentMap) {
      (inputDir / key).write(content)
    }
    (inputDir / "firstJavaFile.java").write("")

    outputDir = File.newTemporaryDirectory()

    val config =
      Config(fetchDependencies = true).withInputPath(inputDir.pathAsString).withOutputPath(outputDir.pathAsString)
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
      "",
      Language.JAVA,
      Array()
    )
  )

  val auditConfigRule = List(
    RuleInfo(
      "AuditCollection.SpringWebMVC",
      "Spring Web MVC",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("^org\\.elasticsearch\\.client:rest$"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.UNKNOWN,
      Constants.auditCollection,
      Language.JAVA,
      Array()
    ),
    RuleInfo(
      "AuditWebClient.http4k",
      "Http client",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("^org\\.http4k:http4k-connect-core$"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.UNKNOWN,
      Constants.auditWebClient,
      Language.JAVA,
      Array()
    ),
    RuleInfo(
      "AuditUtility.Github",
      "Github Utility",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("^com\\.github\\.scala-incubator\\.io:scala-io-file_2.11$"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.UNKNOWN,
      Constants.auditUtility,
      Language.JAVA,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, List(), collectionRule, List(), List(), List(), List(), List(), List(), auditConfigRule)

  val taggerCache = new TaggerCache()
}
