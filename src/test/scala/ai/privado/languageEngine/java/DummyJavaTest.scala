package ai.privado.languageEngine.java

import ai.privado.model.Constants
import io.shiftleft.semanticcpg.language.*
import io.circe.Json
import io.circe.syntax.EncoderOps

class DummyJavaTest extends JavaTestCpgBase {

  "Dummy test to write test case" should {

    val sourceCodeLocation = "src/test/resources/java/dummy"
    "show using only cpg" in {
      val cpg = withCpg(sourceCodeLocation)

      cpg.identifier("firstName").nonEmpty shouldBe true
    }

    "show using output json" in {
      val outputJson = withJson(sourceCodeLocation)

      outputJson(Constants.repoName) shouldBe "dummy".asJson
    }

    "show using cpg with output json" in {
      val (cpg, outputJson) = withCpgAndJson(sourceCodeLocation)

      cpg.identifier("firstName").nonEmpty shouldBe true
      outputJson(Constants.repoName) shouldBe "dummy".asJson
    }
  }

}
