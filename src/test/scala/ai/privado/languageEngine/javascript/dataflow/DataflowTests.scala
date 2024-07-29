package ai.privado.languageEngine.javascript.dataflow

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, DatabaseDetailsCache, RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import ai.privado.utility.StatsRecorder
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.*

class DataflowTests extends JavaScriptFrontendTestSuite {

  private val ruleCache =
    RuleCache().setRule(RuleInfoTestData.rule.copy(sources = RuleInfoTestData.sourceRule, sinks = List(leakageRule)))
  private val auditCache    = AuditCache()
  private val privadoInput  = PrivadoInput()
  private val dataflowCache = new DataFlowCache(privadoInput = privadoInput, auditCache = auditCache)
  private val appCache      = AppCache()

//  "dataflow for derived sources tagged using member name" should {
//    val cpg = code("""class Person {
//        |    constructor(firstName, lastName) {
//        |        this.firstName = firstName;
//        |        this.lastName = lastName;
//        |    }
//        |}
//        |
//        |val userPassword = "123";
//        |console.log(userPassword);
//        |
//        |let p = new Person("", "", "");
//        |console.log(p)
//        |""".stripMargin)
//      .withRuleCache(ruleCache)
//      .withPrivadoInput(privadoInput)
//      .withAuditCache(auditCache)
//      .withAppCache(appCache)
//
//    val dataflowMap =
//      Dataflow(cpg, new StatsRecorder()).dataflow(privadoInput, ruleCache, dataflowCache, auditCache, appCache)
//
//    "contain the original source as the first step" in {
//      val List(firstName, lastName) =
//        dataflowMap
//          .filter((_, path) => path.elements.head.tag.value(s"${Constants.originalSource}_.*").nonEmpty)
//          .map((_, path) => path.elements.head)
//          .toList
//
//      firstName.code shouldBe "this.firstName = firstName;"
//      firstName.lineNumber shouldBe Some(3)
//
//      lastName.code shouldBe "this.lastName = lastName;"
//      lastName.lineNumber shouldBe Some(4)
//    }
//
//    "not impact dataflows not starting from derived sources" in {
//      val List(userPassword) =
//        dataflowMap
//          .filterNot((_, path) => path.elements.head.tag.value(s"${Constants.originalSource}_.*").nonEmpty)
//          .map((_, path) => path.elements.head)
//          .dedup
//          .toList
//
//      userPassword.code shouldBe "userPassword";
//      userPassword.lineNumber shouldBe Some(8)
//    }
//  }

  "dataflow for derived sources tagged using type extensions" should {
    val cpg = code("""class Person {
        |    constructor(firstName, lastName) {
        |        this.firstName = firstName;
        |        this.lastName = lastName;
        |    }
        |}
        |
        |class Man extends Person {
        |   constructor(firstName, lastName) {
        |     super(firstName, lastName)
        |   }
        |}
        |
        |let p = new Man();
        |console.log(p)
        |""".stripMargin)
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)
      .withAuditCache(auditCache)
      .withAppCache(appCache)

    "contain the original source as the first step" in {
      val id   = cpg.identifier("p").l
      val sink = cpg.call("log").l
    }
  }
}
