package ai.privado.threatEngine

import ai.privado.cache.{AuditCache, DataFlowCache, RuleCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.model.*
import ai.privado.passes.{SQLParser, SQLPropertyPass}
import ai.privado.tagger.source.SqlQueryTagger
import better.files.File
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.Map

class ThreatTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  val sourceRule: List[RuleInfo] = List(
    RuleInfo(
      "Data.Sensitive.ContactData.EmailAddress",
      "EmailAddress",
      "",
      Array(),
      List("(?i).*email.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVASCRIPT,
      Array()
    )
  )

  "Validate Threat  PIIShouldNotBePresentInMultipleTables" should {
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
      DataFlow(List(), SourceFilter(Option(true), ""), List()),
      List("**"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    "first use case " in {

      val threatEngine = code("""
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
          |""".stripMargin)

      val result = threatEngine.processProcessingViolations(threat)
      result should not be empty
    }
  }

  def code(code: String): ThreatEngineExecutor = {
    val inputDir = File.newTemporaryDirectory()
    (inputDir / "sample.sql").write(code)
    val outputFile = File.newTemporaryFile()
    val config     = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
    val privadoInput =
      PrivadoInput(generateAuditReport = true, enableAuditSemanticsFilter = true)
    val configAndRules =
      ConfigAndRules(sourceRule, List(), List(), List(), List(), List(), List(), List(), List(), List())
    ScanProcessor.config = privadoInput
    val ruleCache = new RuleCache()
    ruleCache.setRule(configAndRules)
    val cpg           = new JavaSrc2Cpg().createCpgWithOverlays(config).get
    val auditCache    = new AuditCache
    val dataFlowCache = new DataFlowCache(auditCache)

    X2Cpg.applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new SQLParser(cpg, config.inputPath, ruleCache).createAndApply()
    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new Dataflow(cpg).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache)
    val dataFlows: Map[String, Path] = Map()
    new ThreatEngineExecutor(cpg, dataFlows, config.inputPath, ruleCache, null, dataFlowCache, privadoInput)
  }
}
