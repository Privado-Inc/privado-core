package ai.privado.languageEngine.java

import ai.privado.model.Constants
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import io.circe.Json
import io.circe.syntax.EncoderOps

class DummyJavaTest extends JavaFrontendTestSuite {

  "Dummy test to write test case" should {
    val cpg = code("""
        |public class Dummy {
        |    private String firstName;
        |    public Dummy(String firstName) {
        |        this.firstName = firstName;
        |    }
        |    public String getFirstName() {
        |        return firstName;
        |    }
        |    public void setFirstName(String firstName) {
        |        this.firstName = firstName;
        |    }
        |}
        |""".stripMargin).generateScanResult()

    "show using only cpg" in {
      cpg.identifier("firstName").nonEmpty shouldBe true
    }

    "show using output json" in {
      val outputJson = cpg.getPrivadoJson()
      outputJson(Constants.repoName).asString.get should startWith("x2cpgTestTmpDir")
    }
  }
}
