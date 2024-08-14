package ai.privado.languageEngine.php.dataflow

import ai.privado.testfixtures.PhpFrontendTestSuite
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.DataflowExporterValidator
import ai.privado.languageEngine.php.tagger.sink.PhpLeakageValidator
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*

class DataflowTests extends PhpFrontendTestSuite with DataflowExporterValidator with PhpLeakageValidator {

  private val ruleCache =
    RuleCache().setRule(RuleInfoTestData.rule.copy(sources = RuleInfoTestData.sourceRule, sinks = List(leakageRule)))
  private val auditCache    = AuditCache()
  private val privadoInput  = PrivadoInput()
  private val dataflowCache = new DataFlowCache(privadoInput = privadoInput, auditCache = auditCache)
  private val appCache      = AppCache()

  "dataflow for derived sources tagged using member name" should {
    val cpg = code("""<?php
                     |
                     |  class User {
                     |    public $firstName;
                     |    public $lastName;
                     |
                     |    function __construct($fname, $lname) {
                     |      $this->firstName = $fname;
                     |      $this->lastName = $lname;
                     |    }
                     |  }
                     |
                     |  $user = new User("a", "b");
                     |  log->info($user)
                     |
                     |  $userPassword = 123;
                     |  log->info($userPassword);
                     |?>
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

      validateLineNumberForDataflowStep(headDataflowForFirstName, 4)
      validateLineNumberForDataflowStep(headDataflowForLastName, 5)
    }

    "not impact dataflows not starting from derived sources" in {
      val List(userPassword, _) = cpg.identifier.nameExact("userPassword").l

      val List(userPasswordSourceId) = userPassword.tag.name(Constants.id).value.l

      val headDataflowForUserPassword =
        getHeadStepOfDataflow(getDataflowForSourceId(userPasswordSourceId, leakageFlows).get, leakageRule.id).get
      validateLineNumberForDataflowStep(headDataflowForUserPassword, 16)
    }
  }

}
