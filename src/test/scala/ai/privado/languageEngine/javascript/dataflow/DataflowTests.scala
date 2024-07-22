package ai.privado.languageEngine.javascript.dataflow

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.JavaScriptFrontendTestSuite

class DataflowTests extends JavaScriptFrontendTestSuite {
  "dataflow for derived sources" should {
    "abc" in {
      val leakageRule = RuleInfo(
        "Log console",
        "Leakage",
        "",
        FilterProperty.METHOD_FULL_NAME,
        Array(),
        List(".*console.*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        catLevelTwo = Constants.leakages,
        Language.UNKNOWN,
        Array()
      )
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule.copy(sources = RuleInfoTestData.sourceRule, sinks = List(leakageRule))
      )
      val cpg = code("""class Person {
                       |    constructor(firstName, lastName) {
                       |        this.firstName = firstName;
                       |        this.lastName = lastName;
                       |    }
                       |}
                       |
                       |class SomeOtherClass {
                       |    constructor(creditCardNumber) {
                       |        this.creditCardNumber = creditCardNumber;
                       |    }
                       |}
                       |
                       |
                       |let p = new Person("", "", "");
                       |p = "123";
                       |console.log(p)
                       |
                       |let soC = new SomeOtherClass("");
                       |soC = "1233";
                       |console.log(soC);""".stripMargin).withRuleCache(ruleCache)

      println(cpg)

    }
  }
}
