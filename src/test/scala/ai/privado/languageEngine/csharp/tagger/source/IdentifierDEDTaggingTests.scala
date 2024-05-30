package ai.privado.languageEngine.csharp.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, SystemConfig}
import ai.privado.exporter.{DataflowExporterValidator, SourceExporterValidator}
import ai.privado.rule.{DEDRuleTestData, RuleInfoTestData, SinkRuleTestData, SourceRuleTestData}
import ai.privado.testfixtures.CSharpFrontendTestSuite
import io.circe.Json
import io.circe.syntax.EncoderOps

class IdentifierDEDTaggingTests
    extends CSharpFrontendTestSuite
    with SourceExporterValidator
    with DataflowExporterValidator {

  "DED Rule handling with exporter" should {
    val sourceCode = """
                       |using System;
                       |
                       |// Define the User class with required properties
                       |public class User
                       |{
                       |    public string firstName { get; }
                       |    public string passwd { get; }
                       |    public string emailId { get; }
                       |
                       |    public User(string firstName, string passwd, string emailId)
                       |    {
                       |        firstName = firstName;
                       |        passwd = passwd;
                       |        emailId = emailId;
                       |    }
                       |
                       |    public override string ToString()
                       |    {
                       |        return $"User(FirstName={firstName}, Passwd={passwd}, EmailId={emailId})";
                       |    }
                       |}
                       |
                       |class Program
                       |{
                       |    static void Main()
                       |    {
                       |        // Create an instance of the User class
                       |        User user = new User(
                       |            "firstName1",
                       |            "yourPassword",
                       |            "yourEmail@example.com"
                       |        );
                       |
                       |        // Access and print the properties
                       |        Console.WriteLine(user);
                       |    }
                       |}
                       |""".stripMargin

    "should verify if only expected processing & dataflow in privadojson without DED rules" in {
      val ruleCache = RuleCache().setRule(
        RuleInfoTestData.rule
          .copy(sinks = List(SinkRuleTestData.leakageCSharpRule))
      )
      val cpg = code(sourceCode, "Test0.cs").withRuleCache(ruleCache)

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
          .copy(sinks = List(SinkRuleTestData.leakageCSharpRule), dedRules = List(DEDRuleTestData.dedRuleTestCSharp))
      )
      val cpg = code(sourceCode, "Test0.cs").withRuleCache(ruleCache)

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
            sinks = List(SinkRuleTestData.leakageCSharpRule),
            dedRules = List(DEDRuleTestData.dedRuleTestCSharp)
          )
      )
      val cpg = code(sourceCode, "Test0.cs").withRuleCache(ruleCache)

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
