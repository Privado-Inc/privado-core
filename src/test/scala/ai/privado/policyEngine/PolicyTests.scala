package ai.privado.policyEngine

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.exporter.CollectionExporter
import ai.privado.languageEngine.java.tagger.source.InSensitiveCallTagger
import ai.privado.languageEngine.javascript.tagger.collection.CollectionTagger
import ai.privado.languageEngine.javascript.tagger.sink.{JSAPITagger, RegularSinkTagger}
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import ai.privado.model.*
import ai.privado.tagger.collection.WebFormsCollectionTagger
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
      FilterProperty.METHOD_FULL_NAME,
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
      Array(),
      false,
      false
    ),
    RuleInfo(
      "Data.Sensitive.Password",
      "Password",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*password.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVASCRIPT,
      Array(),
      false,
      false
    ),
    RuleInfo(
      "Data.Sensitive.AccountName",
      "Account Name",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*(user[^\\s/(;)#|,=!>]{0,5}name)"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVASCRIPT,
      Array(),
      false,
      false
    )
  )

  val sinkRule: List[RuleInfo] = List(
    RuleInfo(
      "Leakages.Log.Error",
      "Log Error",
      "",
      FilterProperty.METHOD_FULL_NAME,
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
      FilterProperty.METHOD_FULL_NAME,
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
      FilterProperty.METHOD_FULL_NAME,
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

  val collectionRule = List(
    RuleInfo(
      "Collections.Express",
      "Express framework restendpoint",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(".*(express).*(post|get|all|delete|put|patch|head|subscribe|unsubscribe)"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = Constants.default,
      Language.JAVASCRIPT,
      Array()
    ),
    RuleInfo(
      "Collections.Webforms",
      "Webform data collection",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(
        "^<(?i)(?:\\w{0,}(input|upload)\\w{0,}|\\w{0,}(textarea|Text|TextBox|Select|Field|Autocomplete|Checkbox))[^>]*.*"
      ),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      "webforms",
      Language.DEFAULT,
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
        SourceFilter(Option(true), "", "", AllowedSourceFilters(List[String]())),
        List("Leakages.Log.*"),
        SinkFilter(List[String](), "", "", AllowedSinkFilters(List[String]())),
        CollectionFilter("", "")
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
        SourceFilter(Option(true), "", "", AllowedSourceFilters(List[String]())),
        List(".*"),
        SinkFilter(List[String](), "ThirdParties.API", "", AllowedSinkFilters(List[String]())),
        CollectionFilter("", "")
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
        SourceFilter(Option(true), "", "(?i).*email.*", AllowedSourceFilters(List[String]())),
        List(".*"),
        SinkFilter(List[String]("drive.google.com"), "", "", AllowedSinkFilters(List[String]())),
        CollectionFilter("", "")
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

  "Policy Executor: Applying the allowedSourceFilters & allowedSinkFilters" should {
    val policySinkFilter = PolicyOrThreat(
      "Policy.Deny.Sharing.ThirdParties.SDK.Google.Drive",
      "Policy to restrict Contact Information being send to thirdparty sdk",
      "Example: Don't send contact data to thirdparty sdk",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List(".*"),
        SourceFilter(Option(true), "", "", AllowedSourceFilters(List[String]("Data.Sensitive.ContactData"))),
        List(".*"),
        SinkFilter(List[String](), "", "", AllowedSinkFilters(List[String]("drive.google.com"))),
        CollectionFilter("", "")
      ),
      List(".*"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    val policyExecutor = code("""
        |const google = require('googleapis');
        |const axios = require('axios');
        |
        |const username =  "XWXX-2324-KJHH";
        |
        |const accountDetails = {
        |    "ccN": emailAddress
        |};
        |
        |const emailAddress = "jhgjbk@gfdghch";
        |const apiUrl = `https://123.axios.com/todos/${username}`;
        |
        |// Make an API request using Fetch
        |axios.get(apiUrl).then((response) => { return response.json(); })
        |
        |// Create a new instance of the Drive API
        |export const driveObj = google.drive([accountDetails], {version: 'v3', auth });
        |""".stripMargin)

    val List(violationDataflowModel) = policyExecutor.getViolatingFlowsForPolicy(policySinkFilter).toList
    "have a sourceId and sinkId" in {
      violationDataflowModel.sourceId shouldBe "Data.Sensitive.AccountName"
      violationDataflowModel.sinkId shouldBe "Sinks.ThirdParties.API.123.axios.com"
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
        SourceFilter(Option(true), "", "", AllowedSourceFilters(List[String]())),
        List(".*"),
        SinkFilter(List[String]("axios.com"), "", "", AllowedSinkFilters(List[String]())),
        CollectionFilter("", "")
      ),
      List(".*"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    val policyExecutor = code("""
        |const axios = require('axios');
        |const emailAddress = "jhgjbk@gfdghch";
        |const apiUrl = `https://123.axios.com/todos/${emailAddress}`;
        |
        |// Make an API request using Fetch
        |axios.get(apiUrl).then((response) => { return response.json(); })
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

  "Policy Executor: Collection Violations (API) with specific endPoint" should {
    val policySinkFilter = PolicyOrThreat(
      "Policy.Deny.Sharing.Collection.API",
      "Policy to restrict PII being collected to API endpoint",
      "Example: Don't collect PII to API endpoint",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List(".*"),
        SourceFilter(Option(false), "", "", AllowedSourceFilters(List[String]())),
        List(".*"),
        SinkFilter(List[String](), "", "", AllowedSinkFilters(List[String]())),
        CollectionFilter("api", "/v1/auth0/user/reset-pass")
      ),
      List(".*"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    val policyExecutor = code("""
        |import ServerRoute from "@hapi/hapi";
        |
        |interface Auth0UserHandler {
        |    resetPassword: (password) => string;
        |    updateEmail: (emailId: string) => string;
        |}
        |
        |export const v1Auth0UserRoutes = (handler: Auth0UserHandler): ServerRoute[] => [
        |    {
        |        method: "POST",
        |        url: "/v1/auth0/user/reset-password",
        |        handler: (password) => {
        |            handler.resetPassword(password);
        |        },
        |        options: {
        |            description: "Triggers a password reset email to be send to the provided email address.",
        |            notes: ["Will return a success regardless if the email address is valid or not"],
        |            auth: false
        |        },
        |    },
        |    {
        |        method: "GET",
        |        path: "/v1/auth0/user/update-email",
        |        handler: (emailId, userName) => {
        |            console.log(emailId, userName);
        |        },
        |        options: {
        |            description: "ADMIN ONLY. Updates the user's email address in Auth0 _only_.",
        |            notes: ["To change a user's email address everywhere, use the profile-service endpoints"]
        |        },
        |    }
        |]
        |""".stripMargin)

    val List(violationDataflowModel) = policyExecutor.getCollectionFlowsForPolicy(policySinkFilter).toList
    "have a sourceId and violation dataflow model matched" in {
      violationDataflowModel.sourceId shouldBe "Data.Sensitive.Password"
      violationDataflowModel.detail shouldBe Some("\"/v1/auth0/user/reset-password\"")
      violationDataflowModel.occurrence.get.sample shouldBe "password"
      violationDataflowModel.occurrence.get.lineNumber shouldBe 13
      violationDataflowModel.occurrence.get.columnNumber shouldBe 18
      violationDataflowModel.occurrence.get.fileName shouldBe "sample.js"
    }
  }

  "Policy Executor: Collections API Violations with source filters applied" should {
    val policySinkFilter = PolicyOrThreat(
      "Policy.Deny.Collecting.AccountPassword.API",
      "Policy to restrict Account Password being collected to API endpoint",
      "Example: Don't process Account Password",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List("**"),
        SourceFilter(Option(false), "", "", AllowedSourceFilters(List[String]())),
        List(),
        SinkFilter(List[String](), "", "", AllowedSinkFilters(List[String]())),
        CollectionFilter("", "")
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
        |interface Auth0UserHandler {
        |    resetPassword: (password) => string;
        |    updateEmail: (emailId: string) => string;
        |}
        |axios.get(slack_web_url, { "email": "XXXXXXXX"  })
        |  .then((response: Auth0UserHandler) => {
        |    console.log(response);
        |  })
        |  .catch((error) => {
        |    console.error(error);
        |  });
        |""".stripMargin)
    "have a processing sourceId matched" in {
      val processingViolations = policyExecutor.getSourcesMatchingRegexForProcessing(policySinkFilter).toList
      processingViolations shouldBe List("Data.Sensitive.Password")
    }
  }

  "Policy Executor: Collection Violations (Form)" should {
    val policySinkFilter = PolicyOrThreat(
      "Policy.Deny.Sharing.Collection.Form",
      "Policy to restrict PII being collected from Form",
      "Example: Don't collect PII from Forms",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List("**"),
        SourceFilter(Option(false), "", "", AllowedSourceFilters(List[String]())),
        List(".*"),
        SinkFilter(List[String](), "", "", AllowedSinkFilters(List[String]())),
        CollectionFilter("form", "")
      ),
      List(".*"),
      Map[String, String](),
      Map[String, String](),
      "",
      Array[String]()
    )

    val policyExecutor = code("""
        |import React from "react";
        |import { Container } from "react-bootstrap";
        |import { useAuthState } from "react-firebase-hooks/auth";
        |import { useNavigate, useParams } from "react-router-dom";
        |import { toast } from 'react-toastify';
        |import PageTitle from "../../../Components/Shared/PageTitle/PageTitle";
        |import auth from "../../../firebase.init";
        |import useCourseDetails from "../../../hooks/useCourseDetails/useCourseDetails";
        |import "./CheckOut.css";
        |
        |const CheckOut = () => {
        |  const { serviceId } = useParams();
        |  const [course] = useCourseDetails(serviceId);
        |  const [user] = useAuthState(auth);
        |  const navigate = useNavigate();
        |  const handlePlaceOrder = event =>{
        |    event.preventDefault();
        |  }
        |
        |  return (
        |    <Container className="w-50 mx-auto">
        |      <PageTitle title="CheckOut"></PageTitle>
        |              <form onSubmit={handlePlaceOrder}>
        |                <input
        |                  className="form-control"
        |                  type="text"
        |                  name="name"
        |                  value={user.displayName}
        |                  required readOnly
        |                />
        |
        |                <input
        |                  className="form-control"
        |                  type="email"
        |                  name="email"
        |                  value={user.email}
        |                  placeholder="E-mail Address"
        |                  required readOnly
        |                />
        |                <input
        |                  className="form-control"
        |                  type="number"
        |                  name="phone"
        |                  autoComplete="off"
        |                  required
        |                  placeholder="01xxxxxxxxx"
        |                />
        |                <input id='submit' className="btn-danger fw-bold mt-3 w-50" value='Place Order' type="submit" />
        |              </form>
        |    </Container>
        |  );
        |};
        |
        |export default CheckOut;
        |""".stripMargin)

    val List(violationDataflowModel) = policyExecutor.getCollectionFlowsForPolicy(policySinkFilter).toList
    "have a sourceId and violation dataflow model matched" in {
      violationDataflowModel.sourceId shouldBe "Data.Sensitive.ContactData.EmailAddress"
      violationDataflowModel.detail shouldBe Some("sample.js")
      violationDataflowModel.occurrence.get.sample shouldBe "<input\n                  className=\"form-control\"\n                  type=\"email\"\n                  name=\"email\"\n                  value={user.email}\n                  placeholder=\"E-mail Address\"\n                  required readOnly\n                />"
      violationDataflowModel.occurrence.get.lineNumber shouldBe 33
      violationDataflowModel.occurrence.get.columnNumber shouldBe 16
      violationDataflowModel.occurrence.get.fileName shouldBe "sample.js"
    }
  }

  "Policy Executor: Get only Processing Violations" should {
    val policySinkFilter = PolicyOrThreat(
      "Policy.Deny.Processing.PIIs",
      "Policy to restrict PIIs being processed",
      "Example: Don't process piis",
      "Talk to the Data Protection team: dataprotection@org.com",
      PolicyThreatType.COMPLIANCE,
      PolicyAction.DENY,
      DataFlow(
        List("Data.Sensitive.Password"),
        SourceFilter(Option(false), "", "", AllowedSourceFilters(List[String]())),
        List(),
        SinkFilter(List[String](), "", "", AllowedSinkFilters(List[String]())),
        CollectionFilter("api", "")
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
        |interface Auth0UserHandler {
        |    resetPassword: (password) => string;
        |    updateEmail: (emailId: string) => string;
        |}
        |
        |export const v1Auth0UserRoutes = (handler: Auth0UserHandler): ServerRoute[] => [
        |    {
        |        method: "POST",
        |        url: "/v1/auth0/user/reset-password",
        |        handler: (password) => {
        |            handler.resetPassword(password);
        |        }
        |    }
        |]
        |
        |axios.get(slack_web_url, { "email": "XXXXXXXX"  })
        |  .then((response: Auth0UserHandler) => {
        |    console.log(response);
        |  })
        |  .catch((error) => {
        |    console.error(error);
        |  });
        |""".stripMargin)
    val List(violationDataflowModel) = policyExecutor.getCollectionFlowsForPolicy(policySinkFilter).toList
    "have a sourceId and violation dataflow model matched" in {
      violationDataflowModel.sourceId shouldBe "Data.Sensitive.Password"
      violationDataflowModel.detail shouldBe Some("\"/v1/auth0/user/reset-password\"")
      violationDataflowModel.occurrence.get.sample shouldBe "password"
      violationDataflowModel.occurrence.get.lineNumber shouldBe 14
      violationDataflowModel.occurrence.get.columnNumber shouldBe 18
      violationDataflowModel.occurrence.get.fileName shouldBe "sample.js"
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
      ConfigAndRules(sourceRule, sinkRule, collectionRule, List(), List(), List(), List(), List(), systemConfig, List(), List(), List())
    val ruleCache = new RuleCache()
    ruleCache.setRule(configAndRules)
    val cpg           = new JsSrc2Cpg().createCpgWithAllOverlays(config).get
    val auditCache    = new AuditCache
    val dataFlowCache = new DataFlowCache(privadoInput, auditCache)
    val appCache      = new AppCache()

    X2Cpg.applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new IdentifierTagger(cpg, ruleCache, new TaggerCache()).createAndApply()
    new JSAPITagger(cpg, ruleCache, privadoInput, appCache = appCache).createAndApply()
    new RegularSinkTagger(cpg, ruleCache).createAndApply()
    new WebFormsCollectionTagger(cpg, ruleCache).createAndApply()
    new CollectionTagger(cpg, ruleCache).createAndApply()
    new InSensitiveCallTagger(cpg, ruleCache, new TaggerCache()).createAndApply()
    new Dataflow(cpg).dataflow(privadoInput, ruleCache, dataFlowCache, auditCache, appCache)
    val collectionExporter = new CollectionExporter(cpg, ruleCache, appCache = appCache).getCollections
    val policyExecutor =
      new PolicyExecutor(
        cpg,
        dataFlowCache.getDataflowAfterDedup,
        config.inputPath,
        ruleCache,
        privadoInput,
        collectionExporter,
        appCache = appCache
      )
    policyExecutor
  }
}
