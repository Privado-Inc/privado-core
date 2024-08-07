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
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.shiftleft.codepropertygraph.generated.nodes.AstNode

import scala.collection.mutable

class SourceExporterTest extends JavaFrontendTestSuite with DataflowExporterValidator with SourceExporterValidator {

  implicit val engineContext: EngineContext = new EngineContext()

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
        |   public display(User user) {
        |     System.out.println(user);
        |   }
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
      // Only have leakages dataflow
      val outputJson      = cpg.getPrivadoJson()
      val processingList  = getProcessings(outputJson).flatMap(_.occurrences)
      val leakageDataflow = getLeakageFlows(outputJson)
      // List of First Element of every dataflow
      val dataflowSourceElements = leakageDataflow.flatMap(_.sinks).flatMap(_.paths).map(_.path.headOption.get)

      dataflowSourceElements.size shouldBe 1

      // Check first element of every dataflow must have present inside the processing section
      dataflowSourceElements.foreach(headElement => {
        processingList.exists(element => {
          element.sample.equals(headElement.sample) && element.lineNumber.equals(headElement.lineNumber)
        }) shouldBe true
      })
    }
  }

  "when derived node is start node" should {
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

    "tag a derived source" in {
      val derivedSource = cpg.identifier("user").lineNumber(8).l
      derivedSource.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .nonEmpty shouldBe true
    }

    "Processing section should have first Node of every dataflow" in {
      val outputJson             = cpg.getPrivadoJson()
      val processingList         = getProcessings(outputJson).flatMap(_.occurrences)
      val leakageDataflow        = getLeakageFlows(outputJson)
      val dataflowSourceElements = leakageDataflow.flatMap(_.sinks).flatMap(_.paths).map(_.path.headOption.get)

      dataflowSourceElements.size shouldBe 1

      dataflowSourceElements.foreach(headElement => {
        processingList.exists(element => {
          element.sample.equals(headElement.sample) && element.lineNumber.equals(headElement.lineNumber)
        }) shouldBe true
      })
    }
  }

  "when having multiple node tagged with same rule" should {
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

    val outputJson     = cpg.getPrivadoJson()
    val processingList = getProcessings(outputJson)

    "Processing section should have correct Data element Name" in {
      processingList.map(_.sourceId).exists(_.equals("Data.Sensitive.FirstName")) shouldBe true
    }

    "Processing section should have firstNode of dataflow" in {
      val outputJson             = cpg.getPrivadoJson()
      val processingList         = getProcessings(outputJson).flatMap(_.occurrences)
      val leakageDataflow        = getLeakageFlows(outputJson)
      val dataflowSourceElements = leakageDataflow.flatMap(_.sinks).flatMap(_.paths).map(_.path.headOption.get)

      dataflowSourceElements.size shouldBe 2

      dataflowSourceElements.foreach(headElement => {
        processingList.exists(element => {
          element.sample.equals(headElement.sample) && element.lineNumber.equals(headElement.lineNumber)
        }) shouldBe true
      })
    }
  }
}
