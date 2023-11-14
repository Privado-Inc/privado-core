package ai.privado.policyEngine

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.cache.{AuditCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.tagger.source.InSensitiveCallTagger
import ai.privado.languageEngine.javascript.tagger.sink.{JSAPITagger, RegularSinkTagger}
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import ai.privado.model.*
import ai.privado.model.sql.SQLQueryType
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.semantic.Language.*
import io.joern.dataflowengineoss.language.Path
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.collection.mutable

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
    ),
    RuleInfo(
      "ThirdParties.SDK.Google.Drive",
      "Google Drive",
      "",
      Array("drive.google.com"),
      List(
        "(?i)(googleapis|node-google-drive-new|ts-google-drive|selfish-google-drive|google-drive-wrapper|google-drive-connect).*"
      ),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.JAVASCRIPT,
      Array()
    ),
    RuleInfo(
      Constants.thirdPartiesAPIRuleId,
      "Third Party API",
      "",
      Array(),
      List(
        "((?i)((?:http:|https:|ftp:|ssh:|udp:|wss:){0,1}(\\/){0,2}[a-zA-Z0-9_-][^)\\/(#|,!>\\s]{1,50}\\.(?:com|net|org|de|in|uk|us|io|gov|cn|ml|ai|ly|dev|cloud|me|icu|ru|info|top|tk|tr|cn|ga|cf|nl)).*(?<!png|jpeg|jpg|txt|blob|css|html|js|svg))"
      ),
      false,
      "",
      Map(),
      NodeType.API,
      "",
      CatLevelOne.SINKS,
      catLevelTwo = Constants.third_parties,
      Language.JAVASCRIPT,
      Array()
    )
  )

  val systemConfig = List(
    SystemConfig(
      "apiHttpLibraries",
      "(?i)(request|fetch|axios|vue-axios|urllib|http|client|react-query|socket(.){0,1}io|xmlhttprequest|node.http|cors|got|apollo|superagent|wretch|@angular\\/common\\/http|@(.){2,25}\\/http|.*(HttpClient)).*",
      Language.JAVASCRIPT,
      "",
      Array()
    ),
    SystemConfig(
      "apiSinks",
      "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|usequery|fetch|fetchapi|fetchlegacyxml|createfetch|postform|axios|cors|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity|call|createCall|createEndpoint|dispatch|invoke|newMessage|getInput|getOutput|getResponse|marshall|unmarshall|send|asyncSend|emit|on)",
      Language.JAVASCRIPT,
      "",
      Array()
    ),
    SystemConfig(
      "apiIdentifier",
      "(?i).*((hook|base|auth|prov|endpoint|install|cloud|host|request|service|gateway|route|resource|upload|api|worker|tracker)(.){0,12}url|(slack|web)(.){0,4}hook|(sentry|segment)(.){0,1}(dsn)|(rest|api|host|cloud|request|service)(.){0,4}(endpoint|gateway|route)).*",
      Language.JAVASCRIPT,
      "",
      Array()
    ),
    SystemConfig(
      "clientCreationBaseUrlPattern",
      "(?i)(axios.*create|(@angular\\/common\\/http.){0,1}HttpRequest<any>[:]clone)",
      Language.JAVASCRIPT,
      "",
      Array()
    ),
    SystemConfig(
      "ignoredSinks",
      "(?i).*(?<=map|list|jsonobject|json|array|arrays|jsonnode|objectmapper|objectnode).*(put:|get:).*",
      Language.JAVASCRIPT,
      "",
      Array()
    )
  )

  "Policy Executor: Leakages" should {
    val policyLeakages = PolicyOrThreat(
      "Policy.Deny.Sharing.LeakToConsole",
      "Policy to restrict Contact Information being leaked to console",
      "Example: Don't leak contact data",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List("Data.Sensitive.ContactData.*"),
        SourceFilter(Option(true), "", ""),
        List("Leakages.Log.*"),
        SinkFilter(List[String](), "", ""),
        CollectionFilter("")
      ),
      List(".*"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )
    val policyExecutor = code("""
        |let email = "abc@def.com";
        |console.error(email);
        |""".stripMargin)

    val List(violationDataflowModel) = policyExecutor.getViolatingFlowsForPolicy(policyLeakages).toList
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

  "Policy Executor: SinkFilters by sinkType" should {
    val policySinkFilter = PolicyOrThreat(
      "Policy.Deny.Sharing.ThirdParties.API",
      "Policy to restrict Contact Information being send to thirdparty api",
      "Example: Don't send contact data to thirdparty api",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List("Data.Sensitive.ContactData.*"),
        SourceFilter(Option(true), "", ""),
        List(".*"),
        SinkFilter(List[String](), "ThirdParties.API", ""),
        CollectionFilter("")
      ),
      List(".*"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    val policyExecutor = code("""
        |const axios = require('axios');
        |const slack_web_url = 'https://axios.com/todos/1';
        |
        |axios.get(slack_web_url, { "email": "XXXXXXXX"  })
        |  .then((response) => {
        |    console.log(response.data);
        |  })
        |  .catch((error) => {
        |    console.error(error);
        |  });
        |""".stripMargin)

    val List(violationDataflowModel) = policyExecutor.getViolatingFlowsForPolicy(policySinkFilter).toList
    "have a sourceId and sinkId" in {
      violationDataflowModel.sourceId shouldBe "Data.Sensitive.ContactData.EmailAddress"
      violationDataflowModel.sinkId shouldBe "Sinks.ThirdParties.API.axios.com"
    }
    "have non-empty pathIds" in {
      violationDataflowModel.pathIds.size shouldBe 1
    }
    "have only unique path ids" in {
      violationDataflowModel.pathIds.size == violationDataflowModel.pathIds.toSet.size shouldBe true
    }
  }

  "Policy Executor: SinkFilters by domains + SourceFilter by name" should {
    val policySinkFilter = PolicyOrThreat(
      "Policy.Deny.Sharing.ThirdParties.SDK.Google.Drive",
      "Policy to restrict Contact Information being send to thirdparty sdk",
      "Example: Don't send contact data to thirdparty sdk",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List(".*"),
        SourceFilter(Option(true), "", "(?i).*email.*"),
        List(".*"),
        SinkFilter(List[String]("drive.google.com"), "", ""),
        CollectionFilter("")
      ),
      List(".*"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    val policyExecutor = code("""
        |const google = require('googleapis');
        |
        |const emailAddress =  "XWXX-2324-KJHH";
        |
        |const accountDetails = {
        |    "ccN": emailAddress
        |};
        |
        |// Create a new instance of the Drive API
        |export const driveObj = google.drive([accountDetails], {version: 'v3', auth });
        |""".stripMargin)

    val List(violationDataflowModel) = policyExecutor.getViolatingFlowsForPolicy(policySinkFilter).toList
    "have a sourceId and sinkId" in {
      violationDataflowModel.sourceId shouldBe "Data.Sensitive.ContactData.EmailAddress"
      violationDataflowModel.sinkId shouldBe "ThirdParties.SDK.Google.Drive"
    }
    "have non-empty pathIds" in {
      violationDataflowModel.pathIds.size shouldBe 1
    }
    "have only unique path ids" in {
      violationDataflowModel.pathIds.size == violationDataflowModel.pathIds.toSet.size shouldBe true
    }
  }

  "Policy Executor: SinkFilters by domains with regex" should {
    val policySinkFilter = PolicyOrThreat(
      "Policy.Deny.Sharing.ThirdParties.SDK.Google.Drive",
      "Policy to restrict Contact Information being send to thirdparty sdk",
      "Example: Don't send contact data to thirdparty sdk",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List(".*"),
        SourceFilter(Option(true), "", ""),
        List(".*"),
        SinkFilter(List[String]("axios.com"), "", ""),
        CollectionFilter("")
      ),
      List(".*"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    val policyExecutor = code("""
        |const emailAddress = "jhgjbk@gfdghch";
        |const apiUrl = `https://123.axios.com/todos/${emailAddress}`;
        |
        |// Make an API request using Fetch
        |fetch(apiUrl).then((response) => { return response.json(); })
        |""".stripMargin)

    val List(violationDataflowModel) = policyExecutor.getViolatingFlowsForPolicy(policySinkFilter).toList
    "have a sourceId and sinkId" in {
      violationDataflowModel.sourceId shouldBe "Data.Sensitive.ContactData.EmailAddress"
      violationDataflowModel.sinkId shouldBe "Sinks.ThirdParties.API.123.axios.com"
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
      ConfigAndRules(sourceRule, sinkRule, List(), List(), List(), List(), List(), List(), systemConfig, List())
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
    new JSAPITagger(cpg, ruleCache, privadoInput).createAndApply()
    new RegularSinkTagger(cpg, ruleCache).createAndApply()
    new InSensitiveCallTagger(cpg, ruleCache, new TaggerCache()).createAndApply()
    new Dataflow(cpg).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache)

    val policyExecutor = new PolicyExecutor(cpg, dataFlowCache, config.inputPath, ruleCache, privadoInput)
    policyExecutor
  }
}
