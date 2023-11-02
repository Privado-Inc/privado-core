package ai.privado.policyEngine

import ai.privado.cache.{AuditCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.tagger.source.InSensitiveCallTagger
import ai.privado.languageEngine.javascript.tagger.sink.RegularSinkTagger
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import ai.privado.model.*
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PolicyTests extends AnyWordSpec with Matchers with BeforeAndAfterAll {
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

  val sinkRule: List[RuleInfo] = List(
    RuleInfo(
      "Leakages.Log.Error",
      "Log Error",
      "",
      Array(),
      List("(?i).*(error).*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.JAVASCRIPT,
      Array()
    )
  )

  val policy = PolicyOrThreat(
    "Policy.Deny.Sharing.LeakToConsole",
    "Policy to restrict Contact Information being leaked to console",
    "Example: Don't leak contact data",
    "Talk to the Data Protection team: dataprotection@org.com",
    PolicyThreatType.COMPLIANCE,
    PolicyAction.DENY,
    DataFlow(List("Data.Sensitive.ContactData.*"), SourceFilter(Option(true), ""), List("Leakages.Log.*")),
    List(".*"),
    Map[String, String](),
    Map[String, String](),
    "",
    Array[String]()
  )

  "Policy Executor" should {
    val policyExecutor = code("""
        |let email = "abc@def.com";
        |console.error(email);
        |""".stripMargin)

    val List(violationDataflowModel) = policyExecutor.getViolatingFlowsForPolicy(policy).toList
    "have a sourceId and sinkId" in {
      violationDataflowModel.sourceId shouldBe "Data.Sensitive.ContactData.EmailAddress"
      violationDataflowModel.sinkId shouldBe "Leakages.Log.Error"
    }
    "have non-empty pathIds" in {
      violationDataflowModel.pathIds.size shouldBe 1
    }
    "have only unique path ids" in {
      violationDataflowModel.pathIds.size == violationDataflowModel.pathIds.toSet.size shouldBe true
    }
  }

  def code(code: String): PolicyExecutor = {
    val inputDir = File.newTemporaryDirectory()
    (inputDir / "sample.js").write(code)
    val outputFile = File.newTemporaryFile()
    val config     = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
    val privadoInput =
      PrivadoInput(generateAuditReport = true, enableAuditSemanticsFilter = true)
    val configAndRules =
      ConfigAndRules(sourceRule, sinkRule, List(), List(), List(), List(), List(), List(), List(), List())
    ScanProcessor.config = privadoInput
    val ruleCache = new RuleCache()
    ruleCache.setRule(configAndRules)
    val cpg           = new JsSrc2Cpg().createCpgWithAllOverlays(config).get
    val auditCache    = new AuditCache
    val dataFlowCache = new DataFlowCache(auditCache)

    X2Cpg.applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new IdentifierTagger(cpg, ruleCache, new TaggerCache()).createAndApply()
    new RegularSinkTagger(cpg, ruleCache).createAndApply()
    new InSensitiveCallTagger(cpg, ruleCache, new TaggerCache()).createAndApply()
    new Dataflow(cpg).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache)

    val policyExecutor = new PolicyExecutor(cpg, dataFlowCache, config.inputPath, ruleCache, privadoInput)
    policyExecutor
  }
}
