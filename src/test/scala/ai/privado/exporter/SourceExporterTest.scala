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
import ai.privado.exporter.{DataflowExporterValidator, SourceExporterValidator}

class SourceExporterTest extends JavaFrontendTestSuite with DataflowExporterValidator with SourceExporterValidator {

  val ruleCache = RuleCache().setRule(
    RuleInfoTestData.rule
      .copy(sources = RuleInfoTestData.sourceRule, sinks = List(SinkRuleTestData.leakageKotlinRule))
  )

  "Identifier Tagger" should {
    val privadoInput = PrivadoInput(disableDeDuplication = true)
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

    "tag a derived source" in {
      val identifierNodes = cpg.identifier("user").l
      identifierNodes.size shouldBe 1
      identifierNodes.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .nonEmpty shouldBe true
    }

    "export derived source under processing" in {
      val outputJson     = cpg.getPrivadoJson()
      val processingList = getProcessings(outputJson)

      // should be present inside processing section because "user" is the first node of dataflow
      processingList.flatMap(_.occurrences).map(_.sample).exists(_.equals("user")) shouldBe true
    }
  }

  "when derived node is start node" should {
    val privadoInput = PrivadoInput()
    val cpg = code(
      """
        |class User {
        |   public String firstName;
        |}
        |
        |class Main {
        |  public void printValue() {
        |  User user = new User();
        |  System.out.println(user);
        |  }
        |}
        |""".stripMargin,
      "index.java"
    )
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)

    "tag a derived source" in {
      val derivedSource = cpg.identifier("user").lineNumber(8).l
      derivedSource.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .nonEmpty shouldBe true
    }

    "Processing section should have firstNode of every dataflow" in {
      val outputJson     = cpg.getPrivadoJson()
      val processingList = getProcessings(outputJson)

      processingList.headOption.get.occurrences.size shouldBe 2
      processingList.map(_.sourceId).exists(_.equals("Data.Sensitive.FirstName")) shouldBe true

      processingList.flatMap(_.occurrences).map(_.sample).exists(_.equals("user")) shouldBe true
      processingList.flatMap(_.occurrences).map(_.sample).exists(_.equals("java.lang.String firstName")) shouldBe true
    }
  }

  "when having multiple node tagged with same rule" should {
    val privadoInput = PrivadoInput()
    val cpg = code(
      """
        |class Main {
        |
        |   public void printValues() {
        |     String firstName = "first";
        |     String first_name = "first";
        |     System.out.println(firstName);
        |     System.out.println(first_name);
        |   }
        |}
        |""".stripMargin,
      "index.java"
    )
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)

    "Processing section should have firstNode of every dataflow" in {
      val outputJson     = cpg.getPrivadoJson()
      val processingList = getProcessings(outputJson)

      processingList.map(_.sourceId).exists(_.equals("Data.Sensitive.FirstName")) shouldBe true

      processingList.flatMap(_.occurrences).map(_.sample).exists(_.equals("firstName")) shouldBe true
      processingList.flatMap(_.occurrences).map(_.sample).exists(_.equals("first_name")) shouldBe true

    }
  }
}
