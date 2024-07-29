package ai.privado.languageEngine.php.dataflow

import ai.privado.testfixtures.PhpFrontendTestSuite
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*

class DataflowTests extends PhpFrontendTestSuite {

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

    "contain the original source as the first step" in {
      val List(firstName, lastName) =
        dataflowMap
          .filter((_, path) => path.elements.head.tag.value(s"${Constants.originalSource}_.*").nonEmpty)
          .map((_, path) => path.elements.head)
          .dedup
          .toList

      firstName.code shouldBe "$firstName"
      firstName.lineNumber shouldBe Some(4)

      lastName.code shouldBe "$lastName"
      lastName.lineNumber shouldBe Some(5)
    }

    "not impact dataflows not starting from derived sources" in {
      val List(userPassword) =
        dataflowMap
          .filterNot((_, path) => path.elements.head.tag.value(s"${Constants.originalSource}_.*").nonEmpty)
          .map((_, path) => path.elements.head)
          .dedup
          .toList

      userPassword.code shouldBe "$userPassword";
      userPassword.lineNumber shouldBe Some(16)
    }
  }

}
