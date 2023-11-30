package ai.privado.script

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.passes.SQLParser
import better.files.File
import io.circe.Json
import io.joern.javasrc2cpg.Config
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
class ExternalScalaScriptTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  "External Scala script runner" should {
    //TODO To fix the test case `fork := true` was added in build.sbt, which is causing failure in aws codebuild
    "test sample external scala script" ignore {
      val scriptInstance = externalScript("""
                             |import io.shiftleft.codepropertygraph.Cpg
                             |import scala.collection.mutable.LinkedHashMap
                             |import io.circe.*
                             |
                             |new ExternalScript {
                             |  override def process(cpg: Cpg, output: LinkedHashMap[String, Json]): Int = {
                             |    println("External Script Finished.")
                             |    return 1;
                             | }}""".stripMargin)

      scriptInstance.process(new Cpg, mutable.LinkedHashMap[String, Json]()) shouldBe 1;
    }
  }
  def externalScript(code: String): ExternalScript = {
    val inputDir   = File.newTemporaryDirectory()
    val sampleFile = (inputDir / "ExternalScript.scala").write(code)
    val outputFile = File.newTemporaryFile()
    val config     = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
    LoadExternalScript(sampleFile.pathAsString).getFileReference
  }

}
