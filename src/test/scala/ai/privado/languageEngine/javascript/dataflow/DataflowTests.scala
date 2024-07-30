package ai.privado.languageEngine.javascript.dataflow

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.DataflowExporterValidator
import ai.privado.languageEngine.javascript.tagger.sink.JSLeakageValidator
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*

class DataflowTests extends JavaScriptFrontendTestSuite with DataflowExporterValidator with JSLeakageValidator {
  private val ruleCache =
    RuleCache().setRule(RuleInfoTestData.rule.copy(sources = RuleInfoTestData.sourceRule, sinks = List(leakageRule)))
  private val auditCache    = AuditCache()
  private val privadoInput  = PrivadoInput()
  private val dataflowCache = new DataFlowCache(privadoInput = privadoInput, auditCache = auditCache)
  private val appCache      = AppCache()

  "dataflow for derived sources tagged using member name" should {
    val cpg = code("""class Person {
        |    constructor(firstName, lastName) {
        |        this.firstName = firstName;
        |        this.lastName = lastName;
        |    }
        |}
        |
        |val userPassword = "123";
        |console.log(userPassword);
        |
        |let p = new Person("", "", "");
        |console.log(p)
        |""".stripMargin)
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)
      .withAuditCache(auditCache)
      .withAppCache(appCache)

    val dataflowMap =
      Dataflow(cpg, new StatsRecorder()).dataflow(privadoInput, ruleCache, dataflowCache, auditCache, appCache)

    val leakageFlows = getLeakageFlows(cpg.getPrivadoJson())

    "contain the original source as the first step" in {
      val List(firstName, lastName) =
        dataflowMap
          .filter((_, path) => path.elements.head.tag.value(s"${Constants.originalSource}_.*").nonEmpty)
          .map((_, path) => path.elements.head)
          .toList

      val List(firstNameSourceId) = firstName.tag.name("id").value.l
      val List(lastNameSourceId)  = lastName.tag.name("id").value.l

      val headDataflowForFirstName = getHeadStepOfDataflow(getDataflowForSourceId(firstNameSourceId, leakageFlows), leakageRule.id)
      val headDataflowForLastName  = getHeadStepOfDataflow(getDataflowForSourceId(lastNameSourceId, leakageFlows), leakageRule.id)

      validateLineNumberForDataflowStep(headDataflowForFirstName, 3)
      validateLineNumberForDataflowStep(headDataflowForLastName, 4)
    }

    "not impact dataflows not starting from derived sources" in {
      val List(userPassword) =
        dataflowMap
          .filterNot((_, path) => path.elements.head.tag.value(s"${Constants.originalSource}_.*").nonEmpty)
          .map((_, path) => path.elements.head)
          .dedup
          .toList

      val List(userPasswordSourceId) = userPassword.tag.name("id").value.l

      val headDataflowForUserPassword =
        getHeadStepOfDataflow(getDataflowForSourceId(userPasswordSourceId, leakageFlows), leakageRule.id)
      validateLineNumberForDataflowStep(headDataflowForUserPassword, 8)
    }
  }
}
