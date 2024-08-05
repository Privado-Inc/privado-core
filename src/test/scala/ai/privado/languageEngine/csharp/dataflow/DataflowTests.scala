package ai.privado.languageEngine.csharp.dataflow

import ai.privado.testfixtures.CSharpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.EngineContext
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.DataflowExporterValidator
import ai.privado.languageEngine.csharp.tagger.sink.CSharpLeakageValidator
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.traversal.TraversalValidator
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*

class DataflowTests extends CSharpFrontendTestSuite with DataflowExporterValidator with CSharpLeakageValidator with TraversalValidator {
  implicit val engineContext: EngineContext = new EngineContext()

  private val ruleCache =
    RuleCache().setRule(RuleInfoTestData.rule.copy(sources = RuleInfoTestData.sourceRule, sinks = List(leakageRule)))
  private val auditCache    = AuditCache()
  private val privadoInput  = PrivadoInput()
  private val dataflowCache = new DataFlowCache(privadoInput = privadoInput, auditCache = auditCache)
  private val appCache      = AppCache()

  "simple dataflows" should {
    val cpg = code(
      """
          |namespace Foo {
          | public class Bar {
          |   public static void Main() {
          |     int phoneNumber = 123;
          |     Console.WriteLine(phoneNumber);
          |   }
          | }
          |}
          |""".stripMargin,
      "Test.cs"
    )

    "find a path from source to sink through a single step" in {

      val src  = cpg.identifier("phoneNumber").lineNumber(5).l
      val sink = cpg.call.nameExact("WriteLine").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  "dataflow for derived sources tagged using member name" should {
    val cpg = code("""
        |namespace Foo;
        |
        |class Person {
        |   public String firstName { get; set; }
        |   public String lastName { get; set; }
        |}
        |
        |class Runner {
        | static void Main() {
        |   Person p = new Person();
        |   Console.WriteLine(p);
        |
        |   var userPassword = 123;
        |   Console.WriteLine(userPassword);
        |}
        |}
        |""".stripMargin)
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)
      .withAuditCache(auditCache)
      .withAppCache(appCache)

    val dataflowMap =
      Dataflow(cpg, new StatsRecorder()).dataflow(privadoInput, ruleCache, dataflowCache, auditCache, appCache)

    val leakageFlows = getLeakageFlows(cpg.getPrivadoJson())

    "contain the original source as the first step" in {
      val List(firstName) = cpg.member.nameExact("firstName").lineNumber(4).l
      val List(lastName)  = cpg.member.nameExact("lastName").lineNumber(5).l

      val List(firstNameSourceId) = firstName.tag.name(Constants.id).value.l
      val List(lastNameSourceId)  = lastName.tag.name(Constants.id).value.l

      val headDataflowForFirstName =
        getHeadStepOfDataflow(getDataflowForSourceId(firstNameSourceId, leakageFlows).get, leakageRule.id).get
      val headDataflowForLastName =
        getHeadStepOfDataflow(getDataflowForSourceId(lastNameSourceId, leakageFlows).get, leakageRule.id).get

      validateLineNumberForDataflowStep(headDataflowForFirstName, 5)
      validateLineNumberForDataflowStep(headDataflowForLastName, 6)
    }

    "not impact dataflows not starting from derived sources" in {
      val List(userPassword, _) =
        cpg.identifier.nameExact("userPassword").l

      val List(userPasswordSourceId) = userPassword.tag.name(Constants.id).value.l

      val headDataflowForUserPassword =
        getHeadStepOfDataflow(getDataflowForSourceId(userPasswordSourceId, leakageFlows).get, leakageRule.id).get
      validateLineNumberForDataflowStep(headDataflowForUserPassword, 14)
    }
  }

}
