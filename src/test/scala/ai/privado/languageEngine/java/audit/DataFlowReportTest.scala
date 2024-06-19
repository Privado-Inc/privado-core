package ai.privado.languageEngine.java.audit

import ai.privado.audit.DataFlowReport
import ai.privado.cache.{AppCache, DatabaseDetailsCache, SourcePathInfo}
import ai.privado.dataflow.Dataflow
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.tagger.source.*
import ai.privado.model.Language
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.utility.StatsRecorder
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.collection.mutable

class DataFlowReportTest extends DataFlowReportTestBase {

  override val javaFileContentMap: Map[String, String] = getContent()

  override def beforeAll(): Unit = {
    super.beforeAll()
    val appCache             = new AppCache()
    val databaseDetailsCache = new DatabaseDetailsCache()
    appCache.repoLanguage = Language.JAVA

    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    SourceTagger.runTagger(cpg, ruleCache, taggerCache)
    new RegularSinkTagger(cpg, ruleCache, databaseDetailsCache).createAndApply()
    new InSensitiveCallTagger(cpg, ruleCache, taggerCache).createAndApply()
    new Dataflow(cpg, StatsRecorder()).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache, appCache)
  }

  def getContent(): Map[String, String] = {
    val javaFileMap = mutable.HashMap[String, String]()

    javaFileMap.put("Person.java", AuditTestClassData.person)
    javaFileMap.put("Filter2File.java", AuditTestClassData.filter2File)
    javaFileMap.put("Dedup2File.java", AuditTestClassData.dedup2File)
    javaFileMap.put("UserSemantic.java", AuditTestClassData.userSemantic)

    javaFileMap.toMap
  }

  "Data Flow Report" should {
    "Test flow in sheet" in {
      val firstFilterMap  = mutable.HashMap[String, String]()
      val secondFilterMap = mutable.HashMap[String, String]()
      val firstDedupMap   = mutable.HashMap[String, String]()
      val secondDedupMap  = mutable.HashMap[String, String]()

      auditCache.addIntoBeforeSemantics(
        SourcePathInfo("Data.Sensitive.FinancialData.PaymentMode", "ThirdParties.SDK.Stripe", "2880-2883-2882")
      )

      val workflowResult = DataFlowReport.processDataFlowAudit(auditCache)

      workflowResult.foreach(row => {
        firstFilterMap.put(row.head, row(6))
        secondFilterMap.put(row.head, row(7))
        firstDedupMap.put(row.head, row(8))
        secondDedupMap.put(row.head, row(9))
      })

      // check filtering and dedup
      firstFilterMap.contains("Data.Sensitive.FinancialData.PaymentMode") shouldBe true
      firstFilterMap("Data.Sensitive.FinancialData.PaymentMode") shouldBe ("--")
      secondFilterMap("Data.Sensitive.FinancialData.PaymentMode") shouldBe ("--")
      firstDedupMap("Data.Sensitive.FinancialData.PaymentMode") shouldBe ("--")
      secondDedupMap("Data.Sensitive.FinancialData.PaymentMode") shouldBe ("--")
    }

    "test unfiltered flow Data size" in {
      val unfilteredFlow = auditCache.getFlowBeforeFirstFiltering

      unfilteredFlow.size should not equal (0)
    }

    // TODO Flaky test case need to be fixed
    "test Filtering and dedup" ignore {
      val workflowSemanticResult = new mutable.HashMap[SourcePathInfo, String]()
      val workflowFilter1Result  = new mutable.HashMap[SourcePathInfo, String]()
      val workflowFilter2Result  = new mutable.HashMap[SourcePathInfo, String]()
      val workflowdedup1Result   = new mutable.HashMap[SourcePathInfo, String]()
      val workflowdedup2Result   = new mutable.HashMap[SourcePathInfo, String]()
      val workflowfinalResult    = new mutable.HashMap[SourcePathInfo, String]()

      DataFlowReport
        .processDataFlowAudit(auditCache)
        .foreach(row => {
          val sourcePathInfo = SourcePathInfo(row.head, row(1), row(3))
          workflowSemanticResult.put(sourcePathInfo, row(5))
          workflowFilter1Result.put(sourcePathInfo, row(6))
          workflowFilter2Result.put(sourcePathInfo, row(7))
          workflowdedup1Result.put(sourcePathInfo, row(8))
          workflowdedup2Result.put(sourcePathInfo, row(9))
          workflowfinalResult.put(sourcePathInfo, row(10))
        })

      // Check Semantic
      workflowSemanticResult(
        SourcePathInfo(
          "Data.Sensitive.FirstName",
          "Leakages.Log.Info",
          "b -> b -> b -> b -> b -> b -> this -> this.id -> return id; -> RET -> b.getId() -> newValue2 -> newValue2 -> com.test.privado.audit.BaseClass.logger.info(newValue2)"
        )
      ) shouldBe ("YES")
      workflowFilter1Result(
        SourcePathInfo(
          "Data.Sensitive.FirstName",
          "Leakages.Log.Info",
          "b -> b -> b -> b -> b -> b -> this -> this.id -> return id; -> RET -> b.getId() -> newValue2 -> newValue2 -> com.test.privado.audit.BaseClass.logger.info(newValue2)"
        )
      ) shouldBe ("--")

      // Check Filter2
      workflowFilter2Result(
        SourcePathInfo(
          "Data.Sensitive.AccountData.AccountPassword",
          "Leakages.Log.Info",
          "person1 -> person1 -> this -> this.firstName -> return firstName; -> RET -> person1.getFirstName() -> firstName -> firstName -> com.test.privado.audit.Filter2File.logger.info(firstName)"
        )
      ) shouldBe ("YES")
      workflowdedup1Result(
        SourcePathInfo(
          "Data.Sensitive.AccountData.AccountPassword",
          "Leakages.Log.Info",
          "person1 -> person1 -> this -> this.firstName -> return firstName; -> RET -> person1.getFirstName() -> firstName -> firstName -> com.test.privado.audit.Filter2File.logger.info(firstName)"
        )
      ) shouldBe ("--")

      // Check dedup1
      workflowdedup1Result(
        SourcePathInfo(
          "Data.Sensitive.FirstName",
          "Leakages.Log.Info",
          "firstName -> firstName -> com.test.privado.audit.Filter2File.logger.info(firstName)"
        )
      ) shouldBe ("YES")
      workflowdedup2Result(
        SourcePathInfo(
          "Data.Sensitive.FirstName",
          "Leakages.Log.Info",
          "firstName -> firstName -> com.test.privado.audit.Filter2File.logger.info(firstName)"
        )
      ) shouldBe ("--")

      // Check dedup2
      workflowdedup2Result(
        SourcePathInfo(
          "Data.Sensitive.FirstName",
          "Leakages.Log.Info",
          "firstName1 -> firstName1 -> firstName1 + \"value\" -> firstName1 -> firstName1 -> String name -> name -> com.test.privado.audit.Dedup2File.logger.info(name)"
        )
      ) shouldBe ("YES")
      workflowfinalResult(
        SourcePathInfo(
          "Data.Sensitive.FirstName",
          "Leakages.Log.Info",
          "firstName1 -> firstName1 -> firstName1 + \"value\" -> firstName1 -> firstName1 -> String name -> name -> com.test.privado.audit.Dedup2File.logger.info(name)"
        )
      ) shouldBe ("--")
    }
  }
}
