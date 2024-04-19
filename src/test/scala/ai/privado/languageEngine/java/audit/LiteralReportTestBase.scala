package ai.privado.languageEngine.java.audit

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.ConfigAndRules
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class LiteralReportTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val javaFileContentMap: Map[String, String]
  var inputDir: File  = _
  var outputDir: File = _
  val ruleCache       = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- javaFileContentMap) {
      (inputDir / key).write(content)
    }

    outputDir = File.newTemporaryDirectory()

    val config  = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputDir.pathAsString)
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

  val taggerCache = new TaggerCache()
  val rule: ConfigAndRules =
    ConfigAndRules(List(), List(), List(), List(), List(), List(), List(), List(), List(), List())
}
