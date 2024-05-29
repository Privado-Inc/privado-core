package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, SystemConfig}
import ai.privado.exporter.{DataflowExporterValidator, SourceExporterValidator}
import ai.privado.rule.{DEDRuleTestData, RuleInfoTestData, SinkRuleTestData, SourceRuleTestData}
import ai.privado.testfixtures.KotlinFrontendTestSuite
import io.circe.Json
import io.circe.syntax.EncoderOps

class KotlinLanguageExporterTests
    extends KotlinFrontendTestSuite
    with SourceExporterValidator
    with DataflowExporterValidator {

  "DED Rule handling with exporter" should {
    val sourceCode = """
                       |fun main() {
                       |    val firstName = "firstName1"
                       |    val tmp1 = firstName
                       |    val tmp2 = tmp1
                       |    println(tmp2)
                       |
                       |    val first_name = "firstName2"
                       |    val tmp3 = first_name
                       |    println(tmp3)
                       |
                       |    val passwd = "yourPassword"
                       |    val tmp4 = passwd
                       |    println(tmp4)
                       |
                       |    val emailId = "yourEmail@example.com"
                       |    val tmp5 = emailId
                       |    println(tmp5)
                       |}
                       |
                       |""".stripMargin

    "should verify if only expected processing & dataflow in privadojson without DED rules" in {
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(sinks = List(SinkRuleTestData.leakageKotlinRule))
      )
      val cpg = code(sourceCode, "Test0.kt").withRuleCache(ruleCache)

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
          .copy(sinks = List(SinkRuleTestData.leakageKotlinRule), dedRules = List(DEDRuleTestData.dedRuleTestKotlin))
      )
      val cpg = code(sourceCode, "Test0.kt").withRuleCache(ruleCache)

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
            sinks = List(SinkRuleTestData.leakageKotlinRule),
            dedRules = List(DEDRuleTestData.dedRuleTestKotlin)
          )
      )
      val cpg = code(sourceCode, "Test0.kt").withRuleCache(ruleCache)

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
