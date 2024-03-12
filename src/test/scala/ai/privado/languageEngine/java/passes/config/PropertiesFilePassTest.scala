/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.languageEngine.java.passes.config

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.language.*
import ai.privado.model.{
  CatLevelOne,
  ConfigAndRules,
  Constants,
  FilterProperty,
  Language,
  NodeType,
  RuleInfo,
  SystemConfig
}
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, JavaProperty, Literal, Method, MethodParameterIn}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.exporter.HttpConnectionMetadataExporter
import ai.privado.languageEngine.java.tagger.sink.JavaAPITagger
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

class AnnotationTests extends PropertiesFilePassTestBase(".properties") {
  override val configFileContents: String =
    """
      |internal.logger.api.base=https://logger.privado.ai/
      |slack.base.url=https://hooks.slack.com/services/some/leaking/url
      |""".stripMargin

  override val propertyFileContents = ""
  override val codeFileContents: String =
    """
      |
      |import org.springframework.beans.factory.annotation.Value;
      |
      |class Foo {
      |
      |private static String loggerUrl;
      |
      |@Value("${slack.base.url}")
      |private static final String slackWebHookURL;
      |
      |public AuthenticationService(UserRepository userr, SessionsR sesr, ModelMapper mapper,
      |			ObjectMapper objectMapper, @Qualifier("ApiCaller") ExecutorService apiExecutor, SlackStub slackStub,
      |			SendGridStub sgStub, @Value("${internal.logger.api.base}") String loggerBaseURL) {
      |   }
      |
      |@Value("${internal.logger.api.base}")
      |public void setLoggerUrl( String pLoggerUrl )
      |{
      |        loggerUrl = pLoggerUrl;
      |}
      |}
      |""".stripMargin

  "ConfigFilePass" should {
    "connect annotated parameter to property" in {
      val anno: List[AstNode] = cpg.property.usedAt.l
      anno.length shouldBe 3

      anno.code.l shouldBe List(
        "@Value(\"${internal.logger.api.base}\") String loggerBaseURL",
        "java.lang.String loggerUrl",
        "java.lang.String slackWebHookURL"
      )
    }

    "connect property to annotated parameter" in {
      cpg.property.usedAt.originalProperty.l.length shouldBe 3
      cpg.property.usedAt.originalProperty.name.l shouldBe List(
        "internal.logger.api.base",
        "internal.logger.api.base",
        "slack.base.url"
      )
      cpg.property.usedAt.originalProperty.value.l shouldBe List(
        "https://logger.privado.ai/",
        "https://logger.privado.ai/",
        "https://hooks.slack.com/services/some/leaking/url"
      )
    }

    "connect the referenced member to the original property denoted by the annotated method" in {
      cpg.member("loggerUrl").originalProperty.size shouldBe 1
      cpg.member("loggerUrl").originalProperty.name.l shouldBe List("internal.logger.api.base")
      cpg.member("loggerUrl").originalProperty.value.l shouldBe List("https://logger.privado.ai/")
    }
  }
}

class JsonPropertyTests extends PropertiesFilePassTestBase(".json") {
  override val configFileContents = """
      |{
      |    "databases": [
      |      {
      |        "name": "MySQL Database",
      |        "uri": "mysql://username:password@hostname:port/database_name"
      |      }
      |     ],
      |     "mongoUri" : "mongodb://username:password@hostname:port/database_name"
      |}
      |""".stripMargin

  override val codeFileContents = ""

  override val propertyFileContents = ""

  "json file having array nodes" should {
    "get parsed and property nodes should be generated" in {

      new PropertyParserPass(cpg, inputDir.toString(), new RuleCache, Language.JAVASCRIPT).createAndApply()
      cpg.property.map(p => (p.name, p.value)).l shouldBe List(
        ("databases[0].name", "MySQL Database"),
        ("databases[0].uri", "mysql://username:password@hostname:port/database_name"),
        ("mongoUri", "mongodb://username:password@hostname:port/database_name")
      )

    }
  }
}
class GetPropertyTests extends PropertiesFilePassTestBase(".properties") {
  override val configFileContents = """
      |accounts.datasource.url=jdbc:mariadb://localhost:3306/accounts?useSSL=false
      |internal.logger.api.base=https://logger.privado.ai/
      |""".stripMargin
  override val codeFileContents =
    """
      | import org.springframework.core.env.Environment;
      |
      |public class GeneralConfig {
      |   public DataSource dataSource() {
      |     DriverManagerDataSource dataSource = new DriverManagerDataSource();
      |     dataSource.setUrl(env.getProperty("accounts.datasource.url"));
      |     return dataSource;
      |     }
      |}
      |""".stripMargin

  override val propertyFileContents = ""

  "ConfigFilePass" should {
    "create a file node for the property file" in {
      val List(_, _, name: String) = cpg.file.name.l // The default overlays add a new file to cpg.file
      name.endsWith("/test.properties") shouldBe true
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("accounts.datasource.url")
        .contains("jdbc:mariadb://localhost:3306/accounts?useSSL=false") shouldBe true
      properties.get("internal.logger.api.base").contains("https://logger.privado.ai/")
    }

    "connect property nodes to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename.endsWith("/test.properties") shouldBe true
    }

    "connect property node to literal via `IS_USED_AT` edge" in {
      val List(lit: Literal) = cpg.property.usedAt.l
      lit.code shouldBe "\"accounts.datasource.url\""
    }
    "connect literal node to property via `ORIGINAL_PROPERTY` edge" in {
      val List(javaP: JavaProperty) = cpg.property.usedAt.originalProperty.l
      javaP.value shouldBe "jdbc:mariadb://localhost:3306/accounts?useSSL=false"

      val List(lit: Literal) = cpg.property.usedAt.l
      lit.originalProperty.head.value shouldBe "jdbc:mariadb://localhost:3306/accounts?useSSL=false"
      lit.originalPropertyValue.head shouldBe "jdbc:mariadb://localhost:3306/accounts?useSSL=false"
    }
  }
}

class EgressPropertyTests extends PropertiesFilePassTestBase(".yaml") {
  override val configFileContents = """
                                      |spring:
                                      |   application:
                                      |       name: basepath
                                      |mx-record-delete:
                                      |    events:
                                      |      - http:
                                      |          path: /v1/student/{id}
                                      |          method: DELETE
                                      |      - https:
                                      |          path: v1/student/{id}
                                      |          method: GET
                                      |      - ftp:
                                      |          path: student/{id}
                                      |          method: PUT
                                      |      - ssm:
                                      |          path: /
                                      |          method: PUT
                                      |""".stripMargin
  override val codeFileContents =
    """
      | import org.springframework.core.env.Environment;
      |""".stripMargin

  override val propertyFileContents = ""

  "Fetch egress urls from property files" ignore {
    "Check egress urls" in {
      val egressExporter   = HttpConnectionMetadataExporter(cpg, new RuleCache)
      val List(url1, url2) = egressExporter.getEgressUrls
      url1 shouldBe "/v1/student/{id}"
      url2 shouldBe "v1/student/{id}"
    }

    "Check egress urls with single char" in {
      val egressExporter       = HttpConnectionMetadataExporter(cpg, new RuleCache)
      val egressWithSingleChar = egressExporter.getEgressUrls.filter(x => x.size == 1)
      egressWithSingleChar.size shouldBe 0
    }
    "Check application base path" in {
      val httpConnectionMetadataExporter = HttpConnectionMetadataExporter(cpg, new RuleCache)
      val List(basePath)                 = httpConnectionMetadataExporter.getEndPointBasePath
      basePath shouldBe "basepath"
    }
  }
}

// Unit test to check if property is added in the cpg using XML files.
class XMLPropertyTests extends PropertiesFilePassTestBase(".xml") {
  override val configFileContents =
    """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
      |<beans>
      |<bean id="myField" class="com.example.test.GFG">
      |    <property name="staticField" value="${jdbc.url}"/>
      |    <property name="static_two" value="hello-world"/>
      |</bean>
      |<bean id="myField" class="com.example.test.MFM">
      |    <property name="testProperty" ref="myField"/>
      |</bean>
      |</beans>
      |""".stripMargin

  override val propertyFileContents: String =
    """jdbc.url=http://localhost:8081/""".stripMargin
  override val codeFileContents =
    """
      |package com.example.test;
      |
      |import java.util.*;
      |import java.io.*;
      |
      |public class GFG {
      |	private String staticField;
      |}
      |""".stripMargin

  "ConfigFilePass" should {
    "create a file node for the property file" in {
      val List(name: String) = cpg.file.name.l.filter(file => file.endsWith(".xml"))
      name.endsWith("/test.xml") shouldBe true
    }
  }

  "create a `property` node for each property" in {
    val properties = cpg.property.map(x => (x.name, x.value)).toMap
    properties
      .get("static_two")
      .contains("hello-world") shouldBe true
  }

  "Two way edge between member and propertyNode" in {
    val properties = cpg.property.usedAt.originalProperty.l.map(property => (property.name, property.value)).toMap;
    properties
      .get("staticField")
      .contains("http://localhost:8081/") shouldBe true
  }

  "Two way edge between member and propertyNode for no code reference" in {
    val properties = cpg.property.usedAt.originalProperty.l.map(property => (property.name, property.value)).toMap;
    properties
      .contains("static_two") shouldBe false
  }

  "References to another beans should be skipped" in {
    val properties = cpg.property.map(property => (property.name, property.value)).toMap;
    properties
      .contains("testProperty") shouldBe false
  }
}

class YamlPropertyTests2 extends PropertiesFilePassTestBase(".yaml") {
  override val configFileContents = """
      |config:
      |  default:
      |    API_URL: http://exampleKubernetesService
      |""".stripMargin

  override val codeFileContents = """
        |import org.apache.http.HttpResponse;
        |import org.apache.http.client.HttpClient;
        |import org.apache.http.client.methods.HttpGet;
        |import org.apache.http.impl.client.HttpClients;
        |
        |public class APICaller {
        |  private String apiUrl;
        |
        |  public makeCall() {
        |    apiUrl = System.getenv("API_URL");
        |
        |    HttpClient httpClient = HttpClients.createDefault();
        |    HttpGet getRequest = new HttpGet(apiUrl);
        |    HttpResponse response = httpClient.execute(getRequest);
        |  }
        |}
        |""".stripMargin

  override val propertyFileContents: String = ""
  "Http client execute API Sample" should {
    "Http Client execute should be tagged" in {
      val callNode = cpg.call.name("execute").head
      callNode.tag.size shouldBe 6
      callNode.tag
        .nameExact(Constants.id)
        .head
        .value shouldBe (Constants.thirdPartiesAPIRuleId) + ".exampleKubernetesService"
      callNode.tag.nameExact(Constants.catLevelOne).head.value shouldBe Constants.sinks
      callNode.tag.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.third_parties
      callNode.tag.nameExact(Constants.nodeType).head.value shouldBe "api"
      callNode.tag
        .nameExact("third_partiesapi")
        .head
        .value shouldBe (Constants.thirdPartiesAPIRuleId + ".exampleKubernetesService")
      callNode.tag.nameExact("apiUrlSinks.ThirdParties.API.exampleKubernetesService")
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("config.default.API_URL")
        .contains("http://exampleKubernetesService")
    }

    "Two way edge between member and propertyNode" in {
      val properties = cpg.property.usedAt.originalProperty.l.map(property => (property.name, property.value)).toMap
      properties
        .get("config.default.API_URL")
        .contains("http://exampleKubernetesService") shouldBe true
    }
  }
}

/** Base class for tests on properties files and Java code.
  */
// file extension to support for any file as properties
abstract class PropertiesFilePassTestBase(fileExtension: String)
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll {

  var cpg: Cpg = _
  val configFileContents: String
  val codeFileContents: String
  var inputDir: File   = _
  var outputFile: File = _
  val propertyFileContents: String
  val ruleCache = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / s"test$fileExtension").write(configFileContents)

//    (inputDir / "unrelated.file").write("foo")
    if (propertyFileContents.nonEmpty) {
      (inputDir / "application.properties").write(propertyFileContents)
    }
    outputFile = File.newTemporaryFile()

    (inputDir / "GeneralConfig.java").write(codeFileContents)
    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)

    cpg = new JavaSrc2Cpg()
      .createCpg(config)
      .map { cpg =>
        applyDefaultOverlays(cpg)
        cpg
      }
      .get

    ruleCache.setRule(rule)

    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new PropertyParserPass(cpg, inputDir.toString(), new RuleCache, Language.JAVA).createAndApply()
    new JavaPropertyLinkerPass(cpg).createAndApply()
    new JavaYamlLinkerPass(cpg).createAndApply()
    new IdentifierTagger(cpg, ruleCache, TaggerCache()).createAndApply()
    new JavaAPITagger(cpg, ruleCache, PrivadoInput()).createAndApply()

    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

  val systemConfig = List(
    SystemConfig(
      "apiHttpLibraries",
      "(?i)(org.apache.http|okhttp|org.glassfish.jersey|com.mashape.unirest|java.net.http|java.net.URL|org.springframework.(web|core.io)|groovyx.net.http|org.asynchttpclient|kong.unirest.java|org.concordion.cubano.driver.http|javax.net.ssl|javax.xml.soap|org.apache.axis2|com.sun.xml.messaging.saaj|org.springframework.ws.client|com.eviware.soapui|org.apache.cxf|org.jboss.ws|com.ibm.websphere.sca.extensions.soap|com.sun.xml.ws|org.apache.camel.component.cxf|org.codehaus.xfire|org.apache.synapse|org.apache.wink.client|com.oracle.webservices.internal.api.databinding.Databinding|com.sap.engine.interfaces.webservices.runtime.client).*",
      Language.JAVA,
      "",
      Array()
    ),
    SystemConfig(
      "apiSinks",
      "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|fetch|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity|postForObject|call|createCall|createEndpoint|dispatch|invoke|newMessage|getInput|getOutput|getResponse|marshall|unmarshall|send|asyncSend)",
      Language.JAVA,
      "",
      Array()
    ),
    SystemConfig(
      "apiIdentifier",
      "(?i).*((hook|base|auth|prov|endp|install|request|service|gateway|route|resource)(.){0,12}url|(slack|web)(.){0,4}hook|(rest|api|request|service)(.){0,4}(endpoint|gateway|route)).*",
      Language.JAVA,
      "",
      Array()
    ),
    SystemConfig(
      "ignoredSinks",
      "(?i).*(?<=map|list|jsonobject|json|array|arrays|jsonnode|objectmapper|objectnode).*(put:|get:).*",
      Language.JAVA,
      "",
      Array()
    )
  )

  val sinkRule = List(
    RuleInfo(
      Constants.thirdPartiesAPIRuleId,
      "Third Party API",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(
        "(?i)((?:http|https):\\/\\/[a-zA-Z0-9_-][^)\\/(#|,!>\\s]{1,50}\\.\\b(?:com|net|org|de|in|uk|us|io|gov|cn|ml|ai|ly|dev|cloud|me|icu|ru|info|top|tk|tr|cn|ga|cf|nl)\\b).*(?<!png|jpeg|jpg|txt|blob|css|html|js|svg)",
        "(?i).*((hook|base|auth|prov|endp|install|request|service|gateway|route|resource)(.){0,12}url|(slack|web)(.){0,4}hook|(rest|api|request|service)(.){0,4}(endpoint|gateway|route)).*"
      ),
      false,
      "",
      Map(),
      NodeType.API,
      "",
      CatLevelOne.SINKS,
      catLevelTwo = Constants.third_parties,
      Language.JAVA,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(List(), sinkRule, List(), List(), List(), List(), List(), List(), systemConfig, List())

}
