package ai.privado.languageEngine.python.dataflow

import ai.privado.testfixtures.PythonFrontendTestSuite
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.DataflowExporterValidator
import ai.privado.languageEngine.python.tagger.sink.PythonLeakageValidator
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*

class DataflowTests extends PythonFrontendTestSuite with DataflowExporterValidator with PythonLeakageValidator {
  private val ruleCache =
    RuleCache().setRule(RuleInfoTestData.rule.copy(sources = RuleInfoTestData.sourceRule, sinks = List(leakageRule)))
  private val auditCache    = AuditCache()
  private val privadoInput  = PrivadoInput()
  private val dataflowCache = new DataFlowCache(privadoInput = privadoInput, auditCache = auditCache)
  private val appCache      = AppCache()

  "dataflow for derived sources tagged using member name" should {
    val cpg = code("""
        |from person import Person
        |userPassword = "123";
        |print(userPassword);
        |
        |p = Person("", "");
        |print(p)
        |""".stripMargin)
      .moreCode(
        """
          |class Person:
          |    def __init__(self, firstName, lastName):
          |        self.firstName = firstName
          |        self.lastName = lastName
          |""".stripMargin,
        "person.py"
      )
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)
      .withAuditCache(auditCache)
      .withAppCache(appCache)

    val dataflowMap =
      Dataflow(cpg, new StatsRecorder()).dataflow(privadoInput, ruleCache, dataflowCache, auditCache, appCache)

    val leakageFlows = getLeakageFlows(cpg.getPrivadoJson())

    "contain the original source as the first step" in {
      val List(firstName) = cpg.member.nameExact("firstName").l
      val List(lastName)  = cpg.member.nameExact("lastName").l

      val List(firstNameSourceId) = firstName.tag.name("id").value.l
      val List(lastNameSourceId)  = lastName.tag.name("id").value.l

      val headDataflowForFirstName =
        getHeadStepOfDataflow(getDataflowForSourceId(firstNameSourceId, leakageFlows), leakageRule.id)
      val headDataflowForLastName =
        getHeadStepOfDataflow(getDataflowForSourceId(lastNameSourceId, leakageFlows), leakageRule.id)

      validateLineNumberForDataflowStep(headDataflowForFirstName, 4)
      validateLineNumberForDataflowStep(headDataflowForLastName, 5)
    }

    "not impact dataflows not starting from derived sources" in {
      val List(userPassword, _) = cpg.identifier.nameExact("userPassword").l

      val List(userPasswordSourceId) = userPassword.tag.name("id").value.l

      val headDataflowForUserPassword =
        getHeadStepOfDataflow(getDataflowForSourceId(userPasswordSourceId, leakageFlows), leakageRule.id)
      validateLineNumberForDataflowStep(headDataflowForUserPassword, 3)
    }
  }
}
