package ai.privado.languageEngine.java.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.reflect.reify.Taggers

abstract class UnresolvedFlowTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

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

    outputDir = File.newTemporaryDirectory()

    val config  = Config(inputPath = inputDir.toString(), outputPath = outputDir.toString())
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
    ConfigAndRules(sourceRule, sinkRule, List(), List(), List(), List(), List(), List(), List(), List())

  val taggerCache = new TaggerCache()
}
