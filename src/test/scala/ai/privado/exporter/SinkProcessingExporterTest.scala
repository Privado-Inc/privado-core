package ai.privado.exporter

import ai.privado.model.{CatLevelOne, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.model.exporter.SinkProcessingModel
import ai.privado.exporter.DataflowExporterValidator
import ai.privado.testfixtures.JavaFrontendTestSuite
import ai.privado.rule.{RuleInfoTestData, SinkRuleTestData}
import ai.privado.cache.RuleCache
import io.shiftleft.semanticcpg.language.*

class SinkProcessingExporterTest
    extends JavaFrontendTestSuite
    with DataflowExporterValidator
    with SinkExporterValidator {

  val sinkRule = List(
    RuleInfo(
      "Storages.Local",
      "Local Storage",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array("local.com"),
      List("(?i).*(localStorage).*(save|find).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      catLevelTwo = Constants.storages,
      Language.JAVA,
      Array()
    )
  )

  val ruleCache = RuleCache().setRule(
    RuleInfoTestData.rule
      .copy(sources = RuleInfoTestData.sourceRule, sinks = sinkRule)
  )

  "Simple sink processing dataflow Test" should {
    val cpg = code(
      """
        |import Dummy.LocalStorage;
        |
        |class Main {
        |   public void printValues() {
        |     String firstName = "first";
        |
        |     LocalStorage localStorage = new LocalStorage();
        |
        |     localStorage.save(firstName);
        |   }
        |}
        |""".stripMargin,
      "index.java"
    )
      .withRuleCache(ruleCache)

    "Sink processing should have correct node" in {
      val outputJson           = cpg.getPrivadoJson()
      val processingList       = getSinkProcessings(outputJson).flatMap(_.occurrences)
      val storageDataflow      = getStorageFlows(outputJson)
      val dataflowSinkElements = storageDataflow.flatMap(_.sinks).flatMap(_.paths).map(_.path.lastOption.get)

      dataflowSinkElements.foreach(lastElement => {
        processingList.exists(element => {
          element.sample.equals(lastElement.sample) && element.lineNumber.equals(lastElement.lineNumber)
        }) shouldBe true
      })
    }
  }

  "Sink processing test when two sinks tagged with same rule" should {
    val cpg = code(
      """
        |import Dummy.LocalStorage;
        |
        |class Main {
        |   public void printValues() {
        |       String firstName = "name";
        |       String lastName = "last";
        |
        |       LocalStorage localStorage = new LocalStorage();
        |
        |       localStorage.save(firstName);
        |       localStorage.find(lastName);
        |   }
        |}
        |""".stripMargin,
      "index.java"
    )
      .withRuleCache(ruleCache)

    "Sink processing should have correct node" in {
      val outputJson           = cpg.getPrivadoJson()
      val processingList       = getSinkProcessings(outputJson).flatMap(_.occurrences)
      val storageDataflow      = getStorageFlows(outputJson)
      val dataflowSinkElements = storageDataflow.flatMap(_.sinks).flatMap(_.paths).map(_.path.lastOption.get)

      processingList.size shouldBe 2

      dataflowSinkElements.foreach(lastElement => {
        processingList.exists(element => {
          element.sample.equals(lastElement.sample) && element.lineNumber.equals(lastElement.lineNumber)
        }) shouldBe true
      })
    }
  }
}
