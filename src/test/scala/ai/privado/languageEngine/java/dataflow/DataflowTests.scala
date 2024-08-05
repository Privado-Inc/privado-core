package ai.privado.languageEngine.java.dataflow

import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.utility.StatsRecorder
import io.shiftleft.semanticcpg.language.*
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.exporter.DataflowExporterValidator
import ai.privado.languageEngine.java.tagger.sink.JavaLeakageValidator
import ai.privado.testfixtures.JavaFrontendTestSuite

class DataflowTests extends JavaFrontendTestSuite with JavaLeakageValidator with DataflowExporterValidator {
  private val ruleCache =
    RuleCache().setRule(RuleInfoTestData.rule.copy(sources = RuleInfoTestData.sourceRule, sinks = List(leakageRule)))
  private val auditCache    = AuditCache()
  private val privadoInput  = PrivadoInput()
  private val dataflowCache = new DataFlowCache(privadoInput = privadoInput, auditCache = auditCache)
  private val appCache      = AppCache()

  "dataflow for derived sources tagged using member name" should {
    val cpg = code("""
        |package main;
        |
        |public class Person {
        |  public String firstName, lastName;
        |}
        |
        |public class Main {
        | public static void Main(String[] args) {
        |   Person p = new Person();
        |   System.out.println(p);
        |
        |   String userPassword = "123";
        |   System.out.println(userPassword);
        | }
        |
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
      val List(firstName) = cpg.member.nameExact("firstName").lineNumber(5).l
      val List(lastName)  = cpg.member.nameExact("lastName").lineNumber(5).l

      val List(firstNameSourceId) = firstName.tag.name(Constants.id).value.l
      val List(lastNameSourceId)  = lastName.tag.name(Constants.id).value.l

      val headDataflowForFirstName =
        getHeadStepOfDataflow(getDataflowForSourceId(firstNameSourceId, leakageFlows).get, leakageRule.id).get
      val headDataflowForLastName =
        getHeadStepOfDataflow(getDataflowForSourceId(lastNameSourceId, leakageFlows).get, leakageRule.id).get

      validateLineNumberForDataflowStep(headDataflowForFirstName, 5)
      validateLineNumberForDataflowStep(headDataflowForLastName, 5)

    }

    "not impact dataflows not starting from derived sources" in {
      val List(userPassword, _) = cpg.identifier.nameExact("userPassword").l

      val List(userPasswordSourceId) = userPassword.tag.name(Constants.id).value.l

      val headDataflowForUserPassword =
        getHeadStepOfDataflow(getDataflowForSourceId(userPasswordSourceId, leakageFlows).get, leakageRule.id).get
      validateLineNumberForDataflowStep(headDataflowForUserPassword, 13)
    }
  }
}
