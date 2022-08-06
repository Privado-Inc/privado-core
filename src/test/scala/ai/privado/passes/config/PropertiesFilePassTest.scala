package ai.privado.passes.config

import better.files.File
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language._

class PropertiesFilePassTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg : Cpg = _

  private val configFileContents = """
      |server.port=8080
      |spring.application.name=accounts
      |server.servlet.context-path=/accounts
      |""".stripMargin

  "ConfigFilePass" should {
    "create a file node for the property file" in {
      cpg.file.size shouldBe 1
    }
  }

  var inputDir : File = _

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "test.properties").write(configFileContents)
    (inputDir / "unrelated.file").write("foo")
    cpg = Cpg.empty
    new PropertiesFilePass(cpg, inputDir.toString).createAndApply()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    super.afterAll()
  }

}
