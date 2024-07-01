package ai.privado.languageEngine.java.passes.config

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.model.{ConfigAndRules, Language, SystemConfig}
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*
import ai.privado.testfixtures.JavaFrontendTestSuite

import scala.collection.mutable

class PropertyPassFilterTest extends JavaFrontendTestSuite {

  val ruleCache = new RuleCache()

  val systemConfig = List(
    SystemConfig("propertyFileDirCountLimit", "2", Language.JAVA, "", Array()),
    SystemConfig("propertyFileSizeLimit", "1", Language.JAVA, "", Array())
  )

  val rule: ConfigAndRules =
    ConfigAndRules(systemConfig = systemConfig)
  ruleCache.setRule(rule)

  "Test Dir File Filtering" should {
    val cpg = code(
      """
      |dev:
      | key1: value1
      |""".stripMargin,
      "project/pro1.yaml"
    )
      .moreCode(
        """
        |dev:
        | key2: value2
        |""".stripMargin,
        "project/pro2.yaml"
      )
      .moreCode(
        """
        |dev:
        | key3: value3
        |""".stripMargin,
        "project/pro3.yaml"
      )
      .moreCode(
        """
        |dev:
        | key4: value4
        |""".stripMargin,
        "project/config/pro4.yaml"
      )
      .withRuleCache(ruleCache)

    "Test property file filtering" in {
      val propertyNodes = cpg.property.l
      propertyNodes.size shouldBe 1
      propertyNodes.head.name should equal("dev.key4")
      propertyNodes.head.file.head.name.contains("pro4.yaml") shouldBe true
    }
  }

  "Test file size filtering" should {
    val propertyFileContent1 = new StringBuilder("dev:\n")
    for (i <- 1 to 10) {
      propertyFileContent1.append(s"  key$i: value$i\n")
    }

    val propertyFileContent2 = new StringBuilder("prod:\n")
    for (i <- 1 to 500) {
      propertyFileContent2.append(s"  key$i: value$i\n")
    }

    val cpg = code(propertyFileContent1.toString(), "project/pro1.yaml")
      .moreCode(propertyFileContent2.toString(), "project/pro2.yaml")
      .withRuleCache(ruleCache)

    "Test property file filtering" in {
      val propertyNode = cpg.property.l
      propertyNode.size shouldBe 10
      propertyNode.name(".*(prod).*").size shouldBe 0
    }
  }
}
