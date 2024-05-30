package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, SystemConfig}
import ai.privado.exporter.{DataflowExporterValidator, SourceExporterValidator}
import ai.privado.rule.{DEDRuleTestData, RuleInfoTestData, SinkRuleTestData, SourceRuleTestData}
import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import io.circe.Json
import io.circe.syntax.EncoderOps

class JavaScriptLanguageDEDTest
    extends JavaScriptFrontendTestSuite
    with SourceExporterValidator
    with DataflowExporterValidator {

  "DED Rule handling with exporter" should {
    val sourceCode = """
                       |// Define the User class with required properties
                       |class User {
                       |  constructor(firstName, passwd, emailId) {
                       |      this.firstName = firstName;
                       |      this.passwd = passwd;
                       |      this.emailId = emailId;
                       |  }
                       |}
                       |
                       |// Main function to run the program
                       |function main() {
                       |  // Create an instance of the User class
                       |  const user = new User(
                       |      "firstName1",
                       |      "yourPassword",
                       |      "yourEmail@example.com"
                       |  );
                       |
                       |  // Access and print the properties
                       |  console.log(user);
                       |}
                       |""".stripMargin

    "should verify if only expected processing & dataflow in privadojson without DED rules" in {
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(sinks = List(SinkRuleTestData.leakageRule))
      )
      val cpg = code(sourceCode, "Test0.js").withRuleCache(ruleCache)

      val outputJson  = cpg.getPrivadoJson()
      val processings = getProcessings(outputJson)
      val sourceIds   = processings.map((p) => p.sourceId)
      sourceIds shouldBe List("Data.Sensitive.FirstName", "Data.Sensitive.ContactData.EmailAddress")

      val leakageDataflows  = getLeakageFlows(outputJson)
      val dataFlowSourceIds = leakageDataflows.map((lDataflow) => lDataflow.sourceId)
      dataFlowSourceIds shouldBe List("Data.Sensitive.ContactData.EmailAddress", "Data.Sensitive.FirstName")
    }

    "should verify if only expected processing & dataflow in privadojson after DED Rule applied " in {
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(sinks = List(SinkRuleTestData.leakageRule), dedRules = List(DEDRuleTestData.dedRuleTestJS))
      )
      val cpg = code(sourceCode, "Test0.js").withRuleCache(ruleCache)

      val outputJson  = cpg.getPrivadoJson()
      val processings = getProcessings(outputJson)
      val sourceIds   = processings.map((p) => p.sourceId)
      sourceIds shouldBe List("Data.Sensitive.FirstName", "Data.Sensitive.AccountData.AccountPassword")

      val leakageDataflows  = getLeakageFlows(outputJson)
      val dataFlowSourceIds = leakageDataflows.map((lDataflow) => lDataflow.sourceId)
      dataFlowSourceIds shouldBe List("Data.Sensitive.AccountData.AccountPassword", "Data.Sensitive.FirstName")
    }

    "should verify if only expected processing & dataflow in privadojson with external rules + DED Rule applied " in {
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(
            sources = RuleInfoTestData.rule.sources ++ List(SourceRuleTestData.externalEmailSourceRule),
            sinks = List(SinkRuleTestData.leakageRule),
            dedRules = List(DEDRuleTestData.dedRuleTestJS)
          )
      )
      val cpg = code(sourceCode, "Test0.js").withRuleCache(ruleCache)

      val outputJson  = cpg.getPrivadoJson()
      val processings = getProcessings(outputJson)
      val sourceIds   = processings.map((p) => p.sourceId)
      sourceIds shouldBe List(
        "Data.Sensitive.FirstName",
        "Data.Sensitive.AccountData.AccountPassword",
        "Data.Sensitive.ContactData.EmailAddress"
      )

      val leakageDataflows  = getLeakageFlows(outputJson)
      val dataFlowSourceIds = leakageDataflows.map((lDataflow) => lDataflow.sourceId)
      dataFlowSourceIds shouldBe List(
        "Data.Sensitive.AccountData.AccountPassword",
        "Data.Sensitive.ContactData.EmailAddress",
        "Data.Sensitive.FirstName"
      )
    }

  }
}
