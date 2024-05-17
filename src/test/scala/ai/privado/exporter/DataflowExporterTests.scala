package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.sink.JavaLeakageValidator
import ai.privado.model.{Constants, SystemConfig}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.JavaFrontendTestSuite

class DataflowExporterTests extends JavaFrontendTestSuite with JavaLeakageValidator with DataflowExporterValidator {

  "Dataflow exporter limit mechanism" should {

    val sourceCode = """
                       |public class Test {
                       |   public static void main(String[] args){
                       |       String firstName = "firstName1";
                       |       String tmp1 = firstName;
                       |       String tmp2 = tmp1;
                       |       System.out.println(tmp2);
                       |
                       |
                       |       String first_name = "firstName2";
                       |       String tmp3 = first_name;
                       |       System.out.println(tmp3);
                       |   }
                       |}
                       |""".stripMargin

    "validate flow when no limit flag passed" in {
      val systemConfig = List(
        SystemConfig(Constants.dataflowElementInPathLimit, "-1"),
        SystemConfig(Constants.dataflowSourceSinkPairPathLimit, "-1")
      )
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(systemConfig = systemConfig, sinks = RuleInfoTestData.rule.sinks ++ List(leakageRule))
      )
      val cpg = code(sourceCode).withRuleCache(ruleCache)

      val outputMap = cpg.getPrivadoJson()

      val leakageDataflows = getLeakageFlows(outputMap)
      val allFlows         = leakageDataflows.flatMap(_.sinks.flatMap(_.paths))
      allFlows.size shouldBe 2
      allFlows.map(_.path.size) shouldBe List(7, 5)
    }

    "work for limited number elements in a path" in {
      val systemConfig = List(SystemConfig(Constants.dataflowElementInPathLimit, "5"))
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(systemConfig = systemConfig, sinks = RuleInfoTestData.rule.sinks ++ List(leakageRule))
      )
      val cpg = code(sourceCode).withRuleCache(ruleCache)

      val outputMap = cpg.getPrivadoJson()

      val leakageDataflows = getLeakageFlows(outputMap)
      val allFlows         = leakageDataflows.flatMap(_.sinks.flatMap(_.paths))
      allFlows.size shouldBe 1
      allFlows.map(_.path.size) shouldBe List(5)
    }

    "work for limiting number of source-sink pair path" in {
      val systemConfig = List(SystemConfig(Constants.dataflowSourceSinkPairPathLimit, "1"))
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(systemConfig = systemConfig, sinks = RuleInfoTestData.rule.sinks ++ List(leakageRule))
      )
      val cpg = code(sourceCode).withRuleCache(ruleCache)

      val outputMap = cpg.getPrivadoJson()

      val leakageDataflows = getLeakageFlows(outputMap)
      val allFlows         = leakageDataflows.flatMap(_.sinks.flatMap(_.paths))
      allFlows.size shouldBe 1
      allFlows.map(_.path.size) shouldBe List(5) // Because we sort and pick the earlier ones
    }

    "work for limited number elements in a path and get atleast 1 flow, if limit is less" in {
      val systemConfig = List(SystemConfig(Constants.dataflowElementInPathLimit, "2"))
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(systemConfig = systemConfig, sinks = RuleInfoTestData.rule.sinks ++ List(leakageRule))
      )
      val cpg = code(sourceCode).withRuleCache(ruleCache)

      val outputMap = cpg.getPrivadoJson()

      val leakageDataflows = getLeakageFlows(outputMap)
      val allFlows         = leakageDataflows.flatMap(_.sinks.flatMap(_.paths))
      allFlows.size shouldBe 1
      allFlows.map(_.path.size) shouldBe List(5)
    }
  }
}
