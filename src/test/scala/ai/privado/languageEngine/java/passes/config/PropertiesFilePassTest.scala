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

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.semantic.*
import ai.privado.model.Language
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
import ai.privado.testfixtures.JavaFrontendTestSuite
import ai.privado.model.Constants

class AnnotationTests extends JavaFrontendTestSuite {
  "ConfigFilePass" should {
    val cpg = code(
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
        |			SendGridStub sgStub, @Value("${internal.logger.api.base}") String loggerBaseURL, @Named(Constants.MY_ENDPOINT) String endpoint) {
        |   }
        |
        |@Value("${internal.logger.api.base}")
        |public void setLoggerUrl( String pLoggerUrl )
        |{
        |        loggerUrl = pLoggerUrl;
        |}
        |}
        |""".stripMargin,
      "GeneralConfig.java"
    )
      .moreCode(
        """
          |internal.logger.api.base=https://logger.privado.ai/
          |slack.base.url=https://hooks.slack.com/services/some/leaking/url
          |MY_ENDPOINT=http://myservice.com/user
          |""".stripMargin,
        "test.properties"
      )

    "connect annotated parameter to property" in {
      val anno: List[AstNode] = cpg.property.usedAt.l
      anno.length shouldBe 4

      anno.code.l shouldBe List(
        "@Value(\"${internal.logger.api.base}\") String loggerBaseURL",
        "java.lang.String loggerUrl",
        "@Named(Constants.MY_ENDPOINT) String endpoint",
        "java.lang.String slackWebHookURL"
      )
    }

    "connect property to annotated parameter" in {
      cpg.property.usedAt.originalProperty.l.length shouldBe 4
      cpg.property.usedAt.originalProperty.name.l shouldBe List(
        "internal.logger.api.base",
        "internal.logger.api.base",
        "MY_ENDPOINT",
        "slack.base.url"
      )
      cpg.property.usedAt.originalProperty.value.l shouldBe List(
        "https://logger.privado.ai/",
        "https://logger.privado.ai/",
        "http://myservice.com/user",
        "https://hooks.slack.com/services/some/leaking/url"
      )
    }
  }
}

class GetPropertyTests extends JavaFrontendTestSuite {

  "ConfigFilePass" should {
    val cpg = code(
      """
        |accounts.datasource.url=jdbc:mariadb://localhost:3306/accounts?useSSL=false
        |internal.logger.api.base=https://logger.privado.ai/
        |""".stripMargin,
      "test.properties"
    ).moreCode(
      """
        |import org.springframework.core.env.Environment;
        |
        |public class GeneralConfig {
        |   public DataSource dataSource() {
        |     DriverManagerDataSource dataSource = new DriverManagerDataSource();
        |     dataSource.setUrl(env.getProperty("accounts.datasource.url"));
        |     return dataSource;
        |     }
        |}
        |""".stripMargin,
      "GeneralConfig.java"
    )

    "create a file node for the property file" in {
      val List(_, _, name: String) = cpg.file.name.l // The default overlays add a new file to cpg.file
      name shouldBe ("test.properties")
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
      filename shouldBe ("test.properties")
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

class EgressPropertyTests extends JavaFrontendTestSuite {
  val appCache     = new AppCache()
  val privadoInput = PrivadoInput(enableIngressAndEgressUrls = true)

  "Fetch egress urls from property files" should {
    val cpg = code(
      """
        |spring:
        |   application:
        |       name: basepath
        |false-positive-entries:
        |    urls:
        |      - http:
        |          path1: en-wrapper/0.5.6/maven-wrapper-0.5.6.jar
        |          path2: che-maven/3.6.3/apache-maven-3.6.3-bin.zip
        |          path3: dkr.ecr.us-west-2.amazonaws.com/infrastructure/ecr-pusher:latest
        |          path4: mvn -U -P ${ENVIRONMENT} package -DskipTests --settings ${home}/.m2/settings.xml
        |          path5: somename.jpg
        |          path6: somename.png
        |          path7: somename.gif
        |          path8: string having html tags <p>hello</p> and <b>world</b>
        |          path9: /a/b/c containing spaces
        |          path10: github.com/a/b/c
        |          pathe11: ../some/file/path
        |          path12: #somecomment
        |          path13: ///a/b/c
        |          path14: ./some/file/path
        |
        |
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
        |      - privado:
        |          path: https://code.privado.ai/repositories
        |          method: PUT
        |      - privado-without-http:
        |          path: code.privado.ai/repositories
        |          method: PUT
        |""".stripMargin,
      "test.yaml"
    )
      .moreCode(
        """
          |import org.springframework.core.env.Environment;
          |""".stripMargin,
        "GeneralConfig.java"
      )
      .withPrivadoInput(privadoInput)

    "Check egress urls" in {
      val jsonOutput = cpg.getPrivadoJson()
      jsonOutput(Constants.egressUrls).asArray.get.size shouldBe 4
      jsonOutput(Constants.egressUrls).asArray.get.map(_.noSpaces).toList shouldBe List(
        "\"/v1/student/{id}\"",
        "\"v1/student/{id}\"",
        "\"https://code.privado.ai/repositories\"",
        "\"code.privado.ai/repositories\""
      )
    }

    "Check egress urls with single char" in {
      val jsonOutput = cpg.getPrivadoJson()
      jsonOutput(Constants.egressUrls).asArray.get.filter(_.noSpaces.size == 1).toList.size shouldBe 0
    }
    "Check application base path" in {
      val outputJson = cpg.getPrivadoJson()
      outputJson(Constants.httpEndPointBasePaths).asArray.size shouldBe 1
      outputJson(Constants.httpEndPointBasePaths).asArray.get.headOption.get.toString shouldBe "\"basepath\""
    }
  }
}

class XMLPropertyTests extends JavaFrontendTestSuite {
  "ConfigFilePass" should {
    val cpg = code(
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
        |""".stripMargin,
      "test.xml"
    )
      .moreCode("""jdbc.url=http://localhost:8081/""".stripMargin, "application.properties")
      .moreCode(
        """
          |package com.example.test;
          |
          |import java.util.*;
          |import java.io.*;
          |
          |public class GFG {
          |	private String staticField;
          |}
          |""".stripMargin,
        "GeneralConfig.java"
      )

    "create a file node for the property file" in {
      val List(name: String) = cpg.file.name.l.filter(file => file.endsWith(".xml"))
      name shouldBe ("test.xml")
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
}
