package ai.privado.languageEngine.java.audit

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{CatLevelOne, ConfigAndRules, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class DataFlowReportTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val javaFileContentMap: Map[String, String]
  var inputDir: File       = _
  var outputDir: File      = _
  val ruleCache: RuleCache = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- javaFileContentMap) {
      (inputDir / key).write(content)
    }

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
    AppCache.repoLanguage = Language.JAVA
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
    ),
    RuleInfo(
      "Data.Sensitive.AccountData.AccountPassword",
      "AccountPassword",
      "",
      Array(),
      List("(?i).*password.*"),
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

  val sinkRule = List(
    RuleInfo(
      "Leakages.Log.Info",
      "Log Info",
      "",
      Array(),
      List(
        "(?i)(?:org.slf4j.Logger|org.apache.logging.log4j|org.tinylog.Logger|java.util.logging|ch.qos.logback|timber.log.Timber|android.util.Log).*(info|[.]i[:]).*"
      ),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.JAVA,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, sinkRule, collectionRule, List(), List(), List(), List(), List(), List(), List())

  val privadoInput  = PrivadoInput(generateAuditReport = true, enableAuditSemanticsFilter = true)
  val taggerCache   = new TaggerCache()
  val auditCache    = new AuditCache
  val dataFlowCache = new DataFlowCache(privadoInput, auditCache)
}
