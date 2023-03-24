package ai.privado.languageEngine.java.audit

import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class AuditTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val javaFileContentMap: Map[String, String]
  var inputDir: File   = _
  var outputFile: File = _

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- javaFileContentMap) {
      (inputDir / s"$key.java").write(content)
    }
    outputFile = File.newTemporaryDirectory()
    val config = Config(inputPath = inputDir.toString(), outputPath = outputFile.toString())
    cpg = new JavaSrc2Cpg().createCpg(config).get

    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

}
