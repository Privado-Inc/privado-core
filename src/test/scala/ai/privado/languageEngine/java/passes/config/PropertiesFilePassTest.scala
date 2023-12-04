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

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.language.*
import ai.privado.model.Language
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, JavaProperty, Literal, Method, MethodParameterIn}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

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
      |@Value("${slack.base.url}")
      |private static final String slackWebHookURL;
      |
      |public AuthenticationService(UserRepository userr, SessionsR sesr, ModelMapper mapper,
      |			ObjectMapper objectMapper, @Qualifier("ApiCaller") ExecutorService apiExecutor, SlackStub slackStub,
      |			SendGridStub sgStub, @Value("${internal.logger.api.base}") String loggerBaseURL) {
      |   }
      |}
      |""".stripMargin

  "ConfigFilePass" should {
    "connect annotated parameter to property" in {
      val anno: List[AstNode] = cpg.property.usedAt.l
      anno.length shouldBe 2

      anno.foreach(element => {
        element.label match {
          case "METHOD_PARAMETER_IN" =>
            val List(param: MethodParameterIn) = element.toList
            param.name shouldBe "loggerBaseURL"
          case "MEMBER" =>
            element.code shouldBe "java.lang.String slackWebHookURL"
          case _ => s"Unknown label ${element.label}. Test failed"
        }
      })
    }

    "connect property to annotated parameter" in {
      val properties = cpg.property.usedAt.originalProperty.l

      properties.length shouldBe 2

      properties.foreach(prop => {
        prop.name match {
          case "internal.logger.api.base" => prop.value shouldBe ("https://logger.privado.ai/")
          case "slack.base.url"           => prop.value shouldBe ("https://hooks.slack.com/services/some/leaking/url")
          case _                          => s"Unknown value ${prop.value}. Test failed"
        }
      })
    }
  }
}

/* Test for annotation of methods */
class AnnotationMethodTests extends PropertiesFilePassTestBase(".yml") {
  override val configFileContents: String =
    """
      |sample:
      |  url: http://www.somedomain.com/
      |""".stripMargin

  override val propertyFileContents = ""
  override val codeFileContents: String =
    """
      |
      |import org.springframework.beans.factory.annotation.Value;
      |
      |class Foo {
      |
      |@Value("${sample.url}")
      |public void setUrl( String sampleUrl )
      |{
      |    String url = sampleUrl;
      |}
      |}
      |""".stripMargin

  "ConfigFilePass" should {
    "connect annotated method to property" in {
      val anno: List[AstNode] = cpg.property.usedAt.l
      anno.length shouldBe 1

      anno.foreach(element => {
        element.label match {
          case "METHOD" =>
            val List(methodNode: Method) = element.toList
            methodNode.name shouldBe "setUrl"
        }
      })
    }

    "connect property to annotated method" in {
      val properties = cpg.property.usedAt.originalProperty.l
      properties.length shouldBe 1
      properties.foreach(prop => {
        prop.name match {
          case "sample.url" => prop.value shouldBe ("http://www.somedomain.com/")
          case _            => s"Unknown value ${prop.value}. Test failed"
        }
      })
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
      val List(_, name: String) = cpg.file.name.l

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

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / s"test$fileExtension").write(configFileContents)

    (inputDir / "unrelated.file").write("foo")
    if (propertyFileContents.nonEmpty) {
      (inputDir / "application.properties").write(propertyFileContents)
    }
    outputFile = File.newTemporaryFile()

    (inputDir / "GeneralConfig.java").write(codeFileContents)
    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)

    cpg = new JavaSrc2Cpg().createCpg(config).get
    new PropertyParserPass(cpg, inputDir.toString(), new RuleCache, Language.JAVA).createAndApply()
    new JavaPropertyLinkerPass(cpg).createAndApply()

    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

}
