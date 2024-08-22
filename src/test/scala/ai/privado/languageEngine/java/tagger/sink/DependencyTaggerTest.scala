package ai.privado.languageEngine.java.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.exporter.SinkExporterValidator
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.model
import ai.privado.model.*
import ai.privado.model.exporter.{DataFlowSubCategoryPathExcerptModel, SinkProcessingModel}
import ai.privado.testfixtures.JavaFrontendTestSuite

import java.io.File
import scala.collection.mutable

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
      ),
      RuleInfo(
        "ThirdParties.SDK.Google.Sheets",
        "Google Sheets",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("sheets.google.com"),
        List(".*(com.google.apis).*"),
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
      ),
      DependencyInfo(
        "com.google.apis",
        "google-api-services-sheets",
        "v4-rev493-1.23.0",
        "<artifactId>google-api-services-sheets</artifactId>",
        "ThirdParties.SDK.Google.Sheets",
        "Google Sheets",
        List("sheets.google.com"),
        List(),
        42,
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
        |  		<dependency>
        |			<groupId>com.google.apis</groupId>
        |			<artifactId>google-api-services-sheets</artifactId>
        |			<version>v4-rev493-1.23.0</version>
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

    "3p dependency should get added in processing section" in {
      val outjson                                                = cpg.getPrivadoJson()
      val sinkProcessing                                         = getSinkProcessings(outjson)
      val sinkProceMap: mutable.Map[String, SinkProcessingModel] = mutable.Map[String, SinkProcessingModel]()
      sinkProcessing.size shouldBe 2
      sinkProcessing.foreach(sink => {
        sinkProceMap.put(sink.sinkId, sink)
      })

      val twilioSink = sinkProceMap.get("ThirdParties.SDK.twilio")
      twilioSink shouldBe Some(
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

      val gsheetSink = sinkProceMap.get("ThirdParties.SDK.Google.Sheets")
      gsheetSink shouldBe Some(
        SinkProcessingModel(
          sinkId = "ThirdParties.SDK.Google.Sheets",
          occurrences = List(
            DataFlowSubCategoryPathExcerptModel(
              sample = "<artifactId>google-api-services-sheets</artifactId>",
              lineNumber = 42,
              columnNumber = -1,
              fileName = "pom.xml",
              excerpt =
                "\t\t\t<artifactId>spring-boot-starter-test</artifactId>\n\t\t\t<scope>test</scope>\n\t\t</dependency>\n  \t\t<dependency>\n\t\t\t<groupId>com.google.apis</groupId>\n\t\t\t<artifactId>google-api-services-sheets</artifactId> /* <===  */ \n\t\t\t<version>v4-rev493-1.23.0</version>\n\t\t</dependency>\n\t</dependencies>\n\t<build>\n\t\t<plugins>",
              arguments = None
            )
          )
        )
      )
    }
  }

  "Gradle tests" should {
    val dynamicRule = List(
      RuleInfo(
        "ThirdParties.SDK.Google.Play.Services",
        "Google Play Services",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("developers.google.com"),
        List("(?i).*com.google.android.gms.*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "",
        Language.JAVA,
        Array()
      ),
      RuleInfo(
        "ThirdParties.SDK.Firebase.Authentication",
        "Firebase Authentication",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("firebase.google.com"),
        List("(?i).*com.google.firebase.*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "",
        Language.JAVA,
        Array()
      ),
      RuleInfo(
        "ThirdParties.SDK.Firebase",
        "Firebase",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("firebase.google.com"),
        List("(?i).*com.google.firebase.*"),
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
        "com.google.android.gms",
        "play-services-auth",
        "17.0.0",
        "    implementation 'com.google.android.gms:play-services-auth:17.0.0'\n",
        "ThirdParties.SDK.Google.Play.Services",
        "Google Play Services",
        List("developers.google.com"),
        List(),
        36,
        "app/build.gradle"
      ),
      DependencyInfo(
        "com.google.firebase",
        "firebase-auth",
        "19.0.0",
        "    implementation 'com.google.firebase:firebase-auth:19.0.0'\n",
        "ThirdParties.SDK.Firebase.Authentication",
        "Firebase Authentication",
        List("firebase.google.com"),
        List(),
        35,
        "app/build.gradle"
      ),
      DependencyInfo(
        "com.google.firebase",
        "firebase-core",
        "17.1.0",
        "    implementation 'com.google.firebase:firebase-core:17.1.0'\n",
        "ThirdParties.SDK.Firebase",
        "Firebase",
        List("firebase.google.com"),
        List(),
        32,
        "app/build.gradle"
      ),
      DependencyInfo(
        "com.google.gms",
        "google-services",
        "4.2.0",
        "        classpath 'com.google.gms:google-services:4.2.0'\n",
        "ThirdParties.SDK.Google.Play.Services",
        "Google Play Services",
        List("play.google.com"),
        List(),
        11,
        "build.gradle"
      )
    )
    val cpg = code(
      """
        |buildscript {
        |    repositories {
        |        google()
        |        jcenter()
        |        
        |    }
        |    dependencies {
        |        classpath 'com.android.tools.build:gradle:3.5.3'
        |        classpath 'com.google.gms:google-services:4.2.0'
        |
        |        // NOTE: Do not place your application dependencies here; they belong
        |        // in the individual module build.gradle files
        |    }
        |}
        |
        |allprojects {
        |    repositories {
        |        google()
        |        jcenter()
        |        
        |    }
        |}
        |
        |task clean(type: Delete) {
        |    delete rootProject.buildDir
        |}
        |""".stripMargin,
      "build.gradle"
    ).moreCode(
      """
        |apply plugin: 'com.android.application'
        |apply plugin: 'com.google.gms.google-services'
        |
        |android {
        |    compileSdkVersion 29
        |    buildToolsVersion "29.0.1"
        |    defaultConfig {
        |        applicationId "com.example.edu"
        |        minSdkVersion 22
        |        targetSdkVersion 29
        |        versionCode 1
        |        versionName "1.0"
        |        testInstrumentationRunner "androidx.test.runner.AndroidJUnitRunner"
        |    }
        |    buildTypes {
        |        release {
        |            minifyEnabled false
        |            proguardFiles getDefaultProguardFile('proguard-android-optimize.txt'), 'proguard-rules.pro'
        |        }
        |    }
        |}
        |
        |dependencies {
        |    implementation fileTree(dir: 'libs', include: ['*.jar'])
        |    implementation 'androidx.appcompat:appcompat:1.0.2'
        |    implementation 'androidx.constraintlayout:constraintlayout:1.1.3'
        |    implementation 'androidx.cardview:cardview:1.0.0'
        |    implementation 'com.android.support:appcompat-v7:29.0.0'
        |    implementation 'com.android.support:design:29.0.1'
        |    implementation 'androidx.appcompat:appcompat:1.0.0'
        |    implementation 'androidx.core:core:1.0.0'
        |    implementation 'com.google.firebase:firebase-core:17.1.0'
        |    implementation 'com.google.firebase:firebase-auth:19.0.0'
        |    implementation 'com.google.firebase:firebase-database:19.0.0'
        |    implementation 'com.google.firebase:firebase-auth:19.0.0'
        |    implementation 'com.google.android.gms:play-services-auth:17.0.0'
        |    implementation 'com.github.d-max:spots-dialog:1.1@aar'
        |    implementation 'com.google.android.material:material:1.0.0'
        |    implementation 'com.android.volley:volley:1.1.1'
        |    implementation 'com.google.firebase:firebase-messaging:20.1.0'
        |    implementation 'br.com.simplepass:loading-button-android:1.14.0'
        |    implementation 'com.google.android.material:material:1.0.0'
        |    implementation 'com.google.android.material:material:1.1.0-alpha09'
        |    implementation 'com.google.firebase:firebase-storage:16.0.4'
        |    implementation 'androidx.navigation:navigation-fragment:2.0.0'
        |    implementation 'androidx.navigation:navigation-ui:2.0.0'
        |    implementation 'androidx.lifecycle:lifecycle-extensions:2.0.0'
        |
        |    testImplementation 'junit:junit:4.12'
        |    androidTestImplementation 'androidx.test:runner:1.2.0'
        |    androidTestImplementation 'androidx.test.espresso:espresso-core:3.2.0'
        |}
        |""".stripMargin,
      List("app", "build.gradle").mkString(File.separator)
    ).withDependencies(twilioDep)
      .withRuleCache(ruleCache)

    "3p dependency should get added in processing section" in {
      val outjson                                                = cpg.getPrivadoJson()
      val sinkProcessing                                         = getSinkProcessings(outjson)
      val sinkProceMap: mutable.Map[String, SinkProcessingModel] = mutable.Map[String, SinkProcessingModel]()
      sinkProcessing.size shouldBe 3
      sinkProcessing.foreach(sink => {
        sinkProceMap.put(sink.sinkId, sink)
      })

      val playServices = sinkProceMap.get("ThirdParties.SDK.Google.Play.Services")
      playServices shouldBe Some(
        SinkProcessingModel(
          sinkId = "ThirdParties.SDK.Google.Play.Services",
          occurrences = List(
            DataFlowSubCategoryPathExcerptModel(
              sample = "        classpath 'com.google.gms:google-services:4.2.0'\n",
              lineNumber = 11,
              columnNumber = -1,
              fileName = "build.gradle",
              excerpt =
                "        \n    }\n    dependencies {\n        classpath 'com.android.tools.build:gradle:3.5.3'\n        classpath 'com.google.gms:google-services:4.2.0'\n /* <===  */ \n        // NOTE: Do not place your application dependencies here; they belong\n        // in the individual module build.gradle files\n    }\n}\n",
              arguments = None
            ),
            DataFlowSubCategoryPathExcerptModel(
              sample = "    implementation 'com.google.android.gms:play-services-auth:17.0.0'\n",
              lineNumber = 36,
              columnNumber = -1,
              fileName = "app/build.gradle",
              excerpt =
                "    implementation 'androidx.appcompat:appcompat:1.0.0'\n    implementation 'androidx.core:core:1.0.0'\n    implementation 'com.google.firebase:firebase-core:17.1.0'\n    implementation 'com.google.firebase:firebase-auth:19.0.0'\n    implementation 'com.google.firebase:firebase-database:19.0.0'\n    implementation 'com.google.firebase:firebase-auth:19.0.0' /* <===  */ \n    implementation 'com.google.android.gms:play-services-auth:17.0.0'\n    implementation 'com.github.d-max:spots-dialog:1.1@aar'\n    implementation 'com.google.android.material:material:1.0.0'\n    implementation 'com.android.volley:volley:1.1.1'\n    implementation 'com.google.firebase:firebase-messaging:20.1.0'",
              arguments = None
            )
          )
        )
      )

      val firebaseAuth = sinkProceMap.get("ThirdParties.SDK.Firebase.Authentication")
      firebaseAuth shouldBe Some(
        SinkProcessingModel(
          sinkId = "ThirdParties.SDK.Firebase.Authentication",
          occurrences = List(
            DataFlowSubCategoryPathExcerptModel(
              sample = "    implementation 'com.google.firebase:firebase-auth:19.0.0'\n",
              lineNumber = 35,
              columnNumber = -1,
              fileName = "app/build.gradle",
              excerpt =
                "    implementation 'com.android.support:design:29.0.1'\n    implementation 'androidx.appcompat:appcompat:1.0.0'\n    implementation 'androidx.core:core:1.0.0'\n    implementation 'com.google.firebase:firebase-core:17.1.0'\n    implementation 'com.google.firebase:firebase-auth:19.0.0'\n    implementation 'com.google.firebase:firebase-database:19.0.0' /* <===  */ \n    implementation 'com.google.firebase:firebase-auth:19.0.0'\n    implementation 'com.google.android.gms:play-services-auth:17.0.0'\n    implementation 'com.github.d-max:spots-dialog:1.1@aar'\n    implementation 'com.google.android.material:material:1.0.0'\n    implementation 'com.android.volley:volley:1.1.1'",
              arguments = None
            )
          )
        )
      )

      val firebase = sinkProceMap.get("ThirdParties.SDK.Firebase")
      firebase shouldBe Some(
        SinkProcessingModel(
          sinkId = "ThirdParties.SDK.Firebase",
          occurrences = List(
            DataFlowSubCategoryPathExcerptModel(
              sample = "    implementation 'com.google.firebase:firebase-core:17.1.0'\n",
              lineNumber = 32,
              columnNumber = -1,
              fileName = "app/build.gradle",
              excerpt =
                "    implementation 'androidx.constraintlayout:constraintlayout:1.1.3'\n    implementation 'androidx.cardview:cardview:1.0.0'\n    implementation 'com.android.support:appcompat-v7:29.0.0'\n    implementation 'com.android.support:design:29.0.1'\n    implementation 'androidx.appcompat:appcompat:1.0.0'\n    implementation 'androidx.core:core:1.0.0' /* <===  */ \n    implementation 'com.google.firebase:firebase-core:17.1.0'\n    implementation 'com.google.firebase:firebase-auth:19.0.0'\n    implementation 'com.google.firebase:firebase-database:19.0.0'\n    implementation 'com.google.firebase:firebase-auth:19.0.0'\n    implementation 'com.google.android.gms:play-services-auth:17.0.0'",
              arguments = None
            )
          )
        )
      )
    }
  }
}
