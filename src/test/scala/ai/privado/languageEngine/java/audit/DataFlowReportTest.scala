package ai.privado.languageEngine.java.audit

import ai.privado.audit.DataFlowReport
import ai.privado.cache.AuditCache
import ai.privado.cache.AuditCache.SourcePathInfo
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.tagger.source.IdentifierTagger
import ai.privado.entrypoint.ScanProcessor
import ai.privado.tagger.sink.RegularSinkTagger
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.collection.mutable

class DataFlowReportTest extends DataFlowReportTestBase {

  override val javaFileContentMap: Map[String, String] = getContent()

  override def beforeAll(): Unit = {
    super.beforeAll()
    val privadoInput = PrivadoInput(
      Set.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false,
      false
    )

    ScanProcessor.config = privadoInput

    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new RegularSinkTagger(cpg, ruleCache).createAndApply()
    new Dataflow(cpg).dataflow(privadoInput, ruleCache)
  }

  def getContent(): Map[String, String] = {
    val javaFileMap = mutable.HashMap[String, String]()

    javaFileMap.put("Person.java", AuditTestClassData.person)
    javaFileMap.put("Filter2File.java", AuditTestClassData.filter2File)

    javaFileMap.toMap
  }

  "Data Flow Report" should {
    "Test flow in sheet" in {
      val firstFilterMap  = mutable.HashMap[String, String]()
      val secondFilterMap = mutable.HashMap[String, String]()
      val firstDedupMap   = mutable.HashMap[String, String]()
      val secondDedupMap  = mutable.HashMap[String, String]()

      AuditCache.addIntoBeforeFirstFiltering(
        SourcePathInfo("Data.Sensitive.FinancialData.PaymentMode", "ThirdParties.SDK.Stripe", "2880-2883-2882")
      )

      val workflowResult = DataFlowReport.processDataFlowAudit()

      workflowResult.foreach(row => {
        firstFilterMap.put(row.head, row(6))
        secondFilterMap.put(row.head, row(7))
        firstDedupMap.put(row.head, row(8))
        secondDedupMap.put(row.head, row(9))
      })

      // check filtering and dedup
      firstFilterMap.contains("Data.Sensitive.FinancialData.PaymentMode") shouldBe true
      firstFilterMap("Data.Sensitive.FinancialData.PaymentMode") shouldBe ("YES")
      secondFilterMap("Data.Sensitive.FinancialData.PaymentMode") shouldBe ("--")
      firstDedupMap("Data.Sensitive.FinancialData.PaymentMode") shouldBe ("--")
      secondDedupMap("Data.Sensitive.FinancialData.PaymentMode") shouldBe ("--")
    }

    "test unfiltered flow Data size" in {
      val unfilteredFlow = AuditCache.getFlowBeforeFirstFiltering

      unfilteredFlow.size should not equal (0)
    }
  }
}
