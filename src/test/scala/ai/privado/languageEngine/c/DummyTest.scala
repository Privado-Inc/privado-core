package ai.privado.languageEngine.c

import ai.privado.testfixtures.CFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class DummyTest extends CFrontendTestSuite {

  "C frontend should be able to generate a valid CPG" in {

    val cpg = code("""
        |#include <iostream>
        |
        |int main() {
        |    std::cout << "Hello, World!" << std::endl;
        |    return 0;
        |}
        |
        |""".stripMargin)

    cpg.literal.code.l shouldBe List("\"Hello, World!\"", "0")
  }

}
