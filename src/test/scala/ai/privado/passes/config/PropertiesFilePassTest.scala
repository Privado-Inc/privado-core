package ai.privado.passes.config

import better.files.File
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language._
import ai.privado.language._

class PropertiesFilePassTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg : Cpg = _

  private val configFileContents = """
      |server.port=8080
      |spring.application.name=accounts
      |server.servlet.context-path=/accounts
      |""".stripMargin

  "ConfigFilePass" should {
    "create a file node for the property file" in {
      val List(name : String) = cpg.file.name.l
      name.endsWith("/test.properties") shouldBe true
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties.get("server.port").contains("8080") shouldBe true
      properties.get("spring.application.name").contains("accounts") shouldBe true
      properties.get("server.servlet.context-path").contains("/accounts")
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
