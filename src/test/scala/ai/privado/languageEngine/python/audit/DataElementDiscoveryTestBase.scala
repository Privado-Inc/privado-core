package ai.privado.languageEngine.python.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.*
import better.files.File
import io.joern.pysrc2cpg.{Py2CpgOnFileSystem, Py2CpgOnFileSystemConfig}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.compiletime.uninitialized

abstract class DataElementDiscoveryTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = uninitialized
  val pyFileContentMap: Map[String, String]
  var inputDir: File  = uninitialized
  var outputDir: File = uninitialized
  val ruleCache       = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- pyFileContentMap) {
      (inputDir / s"$key.py").write(content)
    }

    outputDir = File.newTemporaryDirectory()

    val excludeFileRegex = ruleCache.getExclusionRegex
    val config = Py2CpgOnFileSystemConfig(Option(File(".venv").path), ignoreVenvDir = true)
      .withInputPath(inputDir.toString)
      .withOutputPath(outputDir.toString)
      .withIgnoredFilesRegex(excludeFileRegex)

    val xtocpg = new Py2CpgOnFileSystem().createCpg(config).map { cpg =>
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
