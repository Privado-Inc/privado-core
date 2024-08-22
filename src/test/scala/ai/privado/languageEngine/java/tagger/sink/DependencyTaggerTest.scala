package ai.privado.languageEngine.java.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.exporter.SinkExporterValidator
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.model
import ai.privado.model.*
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, SinkProcessingModel}
import ai.privado.testfixtures.JavaFrontendTestSuite

class DependencyTaggerTest extends JavaFrontendTestSuite with SinkExporterValidator {

  "Java pom.xml simple use case" should {
    val dynamicRule = List(
      RuleInfo(
        "ThirdParties.SDK.twilio",
        "twilio",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("www.twilio.com"),
        List(".*(com.twilio.sdk).*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "",
        Language.JAVA,
        Array()
      )
    )
    val configAndRule = ConfigAndRules(sinks = dynamicRule)
    val ruleCache     = RuleCache().setRule(configAndRule)
    val twilioDep = List(
      DependencyInfo(
        "com.twilio.sdk",
        "twilio",
        "8.19.1",
        "<artifactId>twilio</artifactId>",
        "ThirdParties.SDK.twilio",
        "Twilio",
        List("www.twilio.com"),
        List(),
        32,
        "pom.xml"
      )
    )
    val cpg = code(
      """
        |<?xml version="1.0" encoding="UTF-8"?>
        |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
        |	<modelVersion>4.0.0</modelVersion>
        |	<parent>
        |		<groupId>org.springframework.boot</groupId>
        |		<artifactId>spring-boot-starter-parent</artifactId>
        |		<version>2.5.4</version>
        |		<relativePath/> <!-- lookup parent from repository -->
        |	</parent>
        |	<groupId>com.atlan</groupId>
        |	<artifactId>SMSService</artifactId>
        |	<version>0.0.1-SNAPSHOT</version>
        |	<name>SMSService</name>
        |	<description>Demo project for Spring Boot</description>
        |	<properties>
        |		<java.version>1.8</java.version>
        |		<spring-cloud.version>2020.0.3</spring-cloud.version>
        |	</properties>
        |	<dependencies>
        | 		<dependency>
        |			<groupId>mysql</groupId>
        |			<artifactId>mysql-connector-java</artifactId>
        |		</dependency>
        |		<dependency>
        |			<groupId>org.springframework.boot</groupId>
        |			<artifactId>spring-boot-starter-web</artifactId>
        |		</dependency>
        |		<dependency>
        |			<groupId>com.twilio.sdk</groupId>
        |			<artifactId>twilio</artifactId>
        |			<version>8.19.1</version>
        |		</dependency>
        |		<dependency>
        |			<groupId>org.springframework.boot</groupId>
        |			<artifactId>spring-boot-starter-test</artifactId>
        |			<scope>test</scope>
        |		</dependency>
        |	</dependencies>
        |	<build>
        |		<plugins>
        |			<plugin>
        |				<groupId>org.springframework.boot</groupId>
        |				<artifactId>spring-boot-maven-plugin</artifactId>
        |			</plugin>
        |		</plugins>
        |	</build>
        |</project>
        |""".stripMargin,
      "pom.xml"
    ).withDependencies(twilioDep).withRuleCache(ruleCache)

    "Twilio dependency should get added in processing section" in {
      val sinkProcessing = getSinkProcessings(cpg.getPrivadoJson())
      sinkProcessing shouldBe List(
        SinkProcessingModel(
          sinkId = "ThirdParties.SDK.twilio",
          occurrences = List(
            DataFlowSubCategoryPathExcerptModel(
              sample = "<artifactId>twilio</artifactId>",
              lineNumber = 32,
              columnNumber = -1,
              fileName = "pom.xml",
              excerpt =
                "\t\t\t<groupId>org.springframework.boot</groupId>\n\t\t\t<artifactId>spring-boot-starter-web</artifactId>\n\t\t</dependency>\n\t\t<dependency>\n\t\t\t<groupId>com.twilio.sdk</groupId>\n\t\t\t<artifactId>twilio</artifactId> /* <===  */ \n\t\t\t<version>8.19.1</version>\n\t\t</dependency>\n\t\t<dependency>\n\t\t\t<groupId>org.springframework.boot</groupId>\n\t\t\t<artifactId>spring-boot-starter-test</artifactId>",
              arguments = None
            )
          )
        )
      )
    }
  }

}
