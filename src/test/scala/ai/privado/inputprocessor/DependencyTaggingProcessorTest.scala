package ai.privado.inputprocessor

import better.files.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DependencyTaggingProcessorTest
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll
    with DependencyTaggingProcessor {

  "Parsing test" should {
    "Working test" in {
      File.usingTemporaryDirectory("deptest") { tmpDir =>
        val tempFile = tmpDir / "java.json";
        tempFile.writeText("""
          |[
          |  {
          |    "groupId": "com.privado.sample",
          |    "dependencyName": "Scanner",
          |    "version": "1.0",
          |    "code" : "<artifactid>Scanner</artifactid>",
          |    "ruleId": "ThirdParties.SDK.Scanner",
          |    "ruleName": "Privado Scanner",
          |    "ruleDomains": [
          |      "scanner.privado.ai"
          |    ],
          |    "ruleTags": [],
          |    "lineNumber": 10,
          |    "filePath": "/path/pom.xml"
          |  }
          |]
          |""".stripMargin)
        val dependencies: List[DependencyInfo] = parseDependencyInfo(tempFile.pathAsString)
        dependencies shouldBe List(
          DependencyInfo(
            "com.privado.sample",
            "Scanner",
            "1.0",
            "<artifactid>Scanner</artifactid>",
            "ThirdParties.SDK.Scanner",
            "Privado Scanner",
            List("scanner.privado.ai"),
            List(),
            10,
            "/path/pom.xml"
          )
        )
      }
    }

    "Graceful handling of error situation" in {
      val dependencies: List[DependencyInfo] = parseDependencyInfo("path/nonexistingfile.json")
      dependencies shouldBe List()
    }
  }

}
