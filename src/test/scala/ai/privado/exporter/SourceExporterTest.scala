package ai.privado.exporter

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.JavaTaggingTestBase
import ai.privado.languageEngine.java.tagger.source.*
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import ai.privado.rule.{RuleInfoTestData, SinkRuleTestData}
import ai.privado.dataflow.Dataflow
import ai.privado.utility.StatsRecorder

class SourceExporterTest extends JavaFrontendTestSuite {

  val ruleCache = RuleCache().setRule(
    RuleInfoTestData.rule
      .copy(sources = RuleInfoTestData.sourceRule, sinks = List(SinkRuleTestData.leakageKotlinRule))
  )

  val auditCache    = AuditCache()
  val privadoInput  = PrivadoInput(disableDeDuplication = true)
  var dataflowCache = new DataFlowCache(privadoInput = privadoInput, auditCache = auditCache)
  var appCache      = AppCache()

  "Identifier Tagger" should {
    val cpg = code(
      """
        |class User {
        |   public String firstName;
        |
        |   public String getFirstName() {return firstName;}
        |   public void setFirstName(String firstName) {this.firstName = firstName;}
        |}
        |
        |class Auth {
        |   public display(User user) {System.out.println(user);}
        |}
        |""".stripMargin,
      "generalFile.java"
    )
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)
      .withAuditCache(auditCache)
      .withAppCache(appCache)

    "tag a derived source" in {
      val identifierNodes = cpg.identifier("user").l
      identifierNodes.size shouldBe 1
      identifierNodes.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .nonEmpty shouldBe true
    }

    "not export derived source under processing" in {
      val dataflowMap =
        Dataflow(cpg, new StatsRecorder()).dataflow(privadoInput, ruleCache, dataflowCache, auditCache, appCache)
      val sourceExporter =
        SourceExporter(
          cpg,
          ruleCache,
          privadoInput,
          appCache = appCache,
          dataFlowCache = dataflowCache,
          dataflows = dataflowMap
        )
      !sourceExporter.getProcessing.flatMap(_.occurrences).map(_.sample).exists(_.equals("user")) shouldBe true
    }
  }

  "Processing source export" should {
    val cpg = code("""
        |class User {
        |   public String firstName;
        |   public String getName() {return firstName;}
        |}
        |
        |class Main {
        |  public void printValue() {
        |  User user = new User();
        |  System.out.println(user);}
        |}
        |""".stripMargin)
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)
      .withAuditCache(auditCache)
      .withAppCache(appCache)

    "tag a derived source" in {
      val derivedSource = cpg.identifier("user").lineNumber(9).l
      derivedSource.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .nonEmpty shouldBe true
    }

    "Processing section should have firstNode of every dataflow" in {
      val dataflowMap =
        Dataflow(cpg, new StatsRecorder()).dataflow(privadoInput, ruleCache, dataflowCache, auditCache, appCache)
      val sourceExporter =
        SourceExporter(
          cpg,
          ruleCache,
          privadoInput,
          appCache = appCache,
          dataFlowCache = dataflowCache,
          dataflows = dataflowMap
        )

      val sourceProcessingModelList = sourceExporter.getProcessing
      sourceProcessingModelList.headOption.get.occurrences.size shouldBe 2

      sourceProcessingModelList.flatMap(_.occurrences).map(_.sample).exists(_.equals("user")) shouldBe true
      sourceProcessingModelList.flatMap(_.occurrences).map(_.sample).exists(_.equals("this.firstName")) shouldBe true

      sourceProcessingModelList.map(_.sourceId).exists(_.equals("Data.Sensitive.FirstName")) shouldBe true
    }
  }
}
