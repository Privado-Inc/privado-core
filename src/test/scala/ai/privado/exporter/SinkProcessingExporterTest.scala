package ai.privado.exporter

import ai.privado.testfixtures.JavaFrontendTestSuite

class SinkProcessingExporterTest extends JavaFrontendTestSuite {

  "Identifier Test" should {
    val cpg = code(
      """
        |class main {
        |   String firstName = "name";
        |   System.out.println(firstName);
        |}
        |""".stripMargin)
  }

}
