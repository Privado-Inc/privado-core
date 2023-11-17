package ai.privado.threatEngine.sql

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.model.*
import ai.privado.threatEngine.ThreatEngineExecutor

import scala.collection.immutable.Map

class ThreatTests extends GoTaggingTestBase {

  "Validate Threat PIIShouldNotBePresentInMultipleTables" should {
    val threat = PolicyOrThreat(
      "PrivadoPolicy.Storage.IsSamePIIShouldNotBePresentInMultipleTables",
      "{DataElement} was found in multiple tables",
      "{DataElement} found in multiple tables",
      """
        |Avoid storing same PII in multiple tables. 
        |Reference link: https://github.com/OWASP/owasp-mstg/blob/v1.4.0/Document/0x05d-Testing-Data-Storage.md#testing-local-storage-for-sensitive-data-mstg-storage-1-and-mstg-storage-2
        |""".stripMargin,
      PolicyThreatType.THREAT,
      PolicyAction.DENY,
      DataFlow(
        List(),
        SourceFilter(Option(true), "", ""),
        List[String](),
        SinkFilter(List[String](), "", ""),
        CollectionFilter("")
      ),
      List("**"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    "When same data-element is part of multiple table in sql file" in {

      val (cpg, threatEngine) = code(
        """
          |CREATE TABLE IF NOT EXISTS Customer (
          |		id SERIAL NOT NULL,
          |		created_at datetime NOT NULL,
          |		email VARCHAR(6) NOT NULL,
          |		PRIMARY KEY (id)
          |	);
          |
          |CREATE TABLE IF NOT EXISTS User (
          |		id SERIAL NOT NULL,
          |		created_at datetime NOT NULL,
          |		email VARCHAR(6) NOT NULL,
          |		PRIMARY KEY (id)
          |	);
          |""".stripMargin,
        ".sql"
      )

      val result = threatEngine.processProcessingViolations(threat)
      result should not be empty
      result.get.policyId shouldBe "PrivadoPolicy.Storage.IsSamePIIShouldNotBePresentInMultipleTables"
      result.get.processing.get.head.sourceId shouldBe "EmailAddress"
    }
  }

}
