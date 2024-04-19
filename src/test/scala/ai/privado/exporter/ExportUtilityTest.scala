package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.model.{ConfigAndRules, Language, SystemConfig}
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class ExportUtilityTest extends JavaFrontendTestSuite {

  val ruleCache = new RuleCache()

  "Test code truncate" should {
    val cpg = code("""
        |public class Dummy {
        |   private String firstName = "random strings";
        |}
        |""".stripMargin).generateScanResult()

    "Test truncate working correctly" in {
      val nodeToTest = cpg.call.methodFullName("<operator>.assignment").head
      ruleCache.withRule(rule)

      val convertedDataflowModel =
        ExporterUtility.convertIndividualPathElement(nodeToTest, 2, 2, "here", new AppCache(), ruleCache)
      convertedDataflowModel.get.sample should equal("String this.fir...")
    }
  }

  val systemConfig = List(SystemConfig("maxCharLimit", "15", Language.JAVA, "", Array()))

  val rule: ConfigAndRules =
    ConfigAndRules(systemConfig = systemConfig)
}
