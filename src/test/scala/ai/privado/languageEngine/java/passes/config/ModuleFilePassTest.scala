package ai.privado.languageEngine.java.passes.config

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.java.language.module.{NodeStarters, StepsForDependency, StepsForModule}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ModuleMavenTest extends ModuleFilePassTestBase {
  override val moduleFileMap: Map[String, String] = getContent()

  private def getContent(): Map[String, String] = {
    val testModuleMap = mutable.HashMap[String, String]()

    val firstContent =
      """ <project xmlns="http://maven.apache.org/POM/4.0.0"
        |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
        |         http://maven.apache.org/maven-v4_0_0.xsd">
        |
        |  <modelVersion>4.0.0</modelVersion>
        |  <groupId>com.example</groupId>
        |  <artifactId>my-java-project</artifactId>
        |  <version>1.0.0</version>
        |
        |  <dependencies>
        |    <dependency>
        |      <groupId>org.springframework</groupId>
        |      <artifactId>spring-core</artifactId>
        |      <version>5.3.9</version>
        |    </dependency>
        |    <dependency>
        |      <groupId>junit</groupId>
        |      <artifactId>junit</artifactId>
        |      <version>4.13.2</version>
        |      <scope>test</scope>
        |    </dependency>
        |  </dependencies>
        |
        |</project>
        |""".stripMargin

    testModuleMap.put("pom.xml", firstContent)
    testModuleMap.toMap
  }

  "MavenModuleInfoPass" should {
    "Test pom file Detection" in {
      val pomFile = cpg.module.file.l

      pomFile.size shouldBe 1
      pomFile.head.name.contains("pom.xml") shouldBe true
    }

    "Test pom.xml content" in {
      val pomFile = cpg.module.l

      pomFile.size shouldBe 1
      pomFile.head.groupid shouldBe "com.example"
      pomFile.head.artifactid.get shouldBe "my-java-project"
      pomFile.head.version shouldBe "1.0.0"
    }
  }

  "MavenModuleDependencyPass" should {
    "Test Maven Dependency List" in {
      val dependencyGroupIdList    = new ListBuffer[String]()
      val dependencyArtifactIdList = new ListBuffer[String]()
      val dependencyVersionList    = new ListBuffer[String]()

      val dependencies = cpg.module.dependencies.l

      dependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid.get
        dependencyVersionList += dependency.version
      })

      dependencies.size shouldBe 2

      // Test Dependency groupId
      dependencyGroupIdList should contain("org.springframework")
      dependencyGroupIdList should contain("junit")

      // Test Dependency artifactId
      dependencyArtifactIdList should contain("spring-core")
      dependencyArtifactIdList should contain("junit")

      // Test Dependency Versions
      dependencyVersionList should contain("5.3.9")
      dependencyVersionList should contain("4.13.2")

    }

    "Test Maven Dependency with filter" in {
      val dependencyGroupIdList    = new ListBuffer[String]()
      val dependencyArtifactIdList = new ListBuffer[String]()
      val dependencyVersionList    = new ListBuffer[String]()

      val filteredDependencies = cpg.module.dependencies.filter(d => d.groupid matches ("org.springframework")).l

      filteredDependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid.get
        dependencyVersionList += dependency.version
      })

      filteredDependencies.size shouldBe 1

      // Test Dependency groupId
      dependencyGroupIdList should contain("org.springframework")
      dependencyGroupIdList should not contain ("junit")

      // Test Dependency artifactId
      dependencyArtifactIdList should contain("spring-core")
      dependencyArtifactIdList should not contain ("junit")

      // Test Dependency Versions
      dependencyVersionList should contain("5.3.9")
      dependencyVersionList should not contain ("4.13.2")
    }

    "Test Maven Dependency File List" in {
      val fileList = cpg.module.dependencies.file.l

      fileList.head.name.contains("pom.xml") shouldBe true
    }
  }
}

class ModuleGradleTest extends ModuleFilePassTestBase {
  override val moduleFileMap: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val testModuleMap = mutable.HashMap[String, String]()

    val firstContent =
      """plugins {
        |	id 'java'
        |	id 'org.springframework.boot' version '3.0.5'
        |	id 'io.spring.dependency-management' version '1.1.0'
        |}
        |
        |group = 'com.test'
        |version = '0.0.1-SNAPSHOT'
        |sourceCompatibility = '17'
        |
        |configurations {
        |	compileOnly {
        |		extendsFrom annotationProcessor
        |	}
        |}
        |
        |repositories {
        |	mavenCentral()
        |}
        |
        |dependencies {
        |	implementation 'com.google.guava:guava:30.1-jre'
        |	compileOnly 'org.projectlombok:lombok:1.2.3'
        |	testImplementation 'org.springframework.boot:spring-boot-starter-test:4.3.2'
        |}
        |
        |tasks.named('test') {
        |	useJUnitPlatform()
        |}
        |
        |""".stripMargin

    testModuleMap.put("build.gradle", firstContent)
    testModuleMap.toMap
  }

  "GradleModuleFilePass" should {
    "Test build.gradle file" in {
      val pomFile = cpg.module.file.l

      pomFile.size shouldBe 1
      pomFile.head.name.contains("build.gradle") shouldBe true
    }

    "Test build.gradle content" in {
      val buildGradleFile = cpg.module.l

      buildGradleFile.size shouldBe 1
      buildGradleFile.head.groupid shouldBe "com.test"
      buildGradleFile.head.version shouldBe "0.0.1-SNAPSHOT"
    }
  }

  "GradleModuleDependencyPass" should {
    "Test Gradle Dependency List" in {
      val dependencyGroupIdList    = new ListBuffer[String]()
      val dependencyArtifactIdList = new ListBuffer[String]()
      val dependencyVersionList    = new ListBuffer[String]()

      val dependencies = cpg.module.dependencies.l

      dependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid.get
        dependencyVersionList += dependency.version
      })

      dependencies.size shouldBe 3

      dependencyGroupIdList should contain("com.google.guava")
      dependencyGroupIdList should contain("org.projectlombok")
      dependencyGroupIdList should contain("org.springframework.boot")

      // Test Dependency artifactId
      dependencyArtifactIdList should contain("guava")
      dependencyArtifactIdList should contain("spring-boot-starter-test")
      dependencyArtifactIdList should contain("lombok")

      // Test Dependency Versions
      dependencyVersionList should contain("30.1-jre")
      dependencyVersionList should contain("1.2.3")
      dependencyVersionList should contain("4.3.2")
    }

    "Test Gradle Dependency with filter" in {
      val dependencyGroupIdList    = new ListBuffer[String]()
      val dependencyArtifactIdList = new ListBuffer[String]()
      val dependencyVersionList    = new ListBuffer[String]()

      val filteredDependencies = cpg.module.dependencies.filter(d => d.groupid matches ("com.google.guava")).l

      filteredDependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid.get
        dependencyVersionList += dependency.version
      })

      filteredDependencies.size shouldBe 1

      dependencyGroupIdList should contain("com.google.guava")
      dependencyGroupIdList should not contain ("org.projectlombok")
      dependencyGroupIdList should not contain ("org.springframework.boot")

      // Test Dependency artifactId
      dependencyArtifactIdList should contain("guava")
      dependencyArtifactIdList should not contain ("spring-boot-starter-test")
      dependencyArtifactIdList should not contain ("lombok")

      // Test Dependency Versions
      dependencyVersionList should contain("30.1-jre")
      dependencyVersionList should not contain ("1.2.3")
      dependencyVersionList should not contain ("4.3.2")
    }

    "Test Gradle Dependency File List" in {
      val fileList = cpg.module.dependencies.file.l

      fileList.head.name.contains("build.gradle") shouldBe true
    }
  }

}

abstract class ModuleFilePassTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg        = _
  var inputDir: File  = _
  var outputDir: File = _
  val moduleFileMap: Map[String, String]

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((fileName, content) <- moduleFileMap) {
      (inputDir / fileName).write(content)
    }

    outputDir = File.newTemporaryDirectory()
    val config  = Config(inputPath = inputDir.toString(), outputPath = outputDir.toString(), fetchDependencies = true)
    val javaSrc = new JavaSrc2Cpg()
    val xtocpg = javaSrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

    cpg = xtocpg.get
    RuleCache.setRule(rule)
    new ModuleFilePass(cpg, inputDir.toString()).createAndApply()

    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputDir.delete()
    super.afterAll()
  }

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      Array(),
      List("(?i).*firstName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVA,
      Array()
    )
  )

  val collectionRule = List(
    RuleInfo(
      "Collections.Annotation.Spring",
      "Spring Web Interface Annotation",
      "",
      Array(),
      List("RequestMapping|PostMapping|PutMapping|GetMapping|DeleteMapping"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      "",
      Language.JAVA,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, List(), collectionRule, List(), List(), List(), List(), List(), List())

  val taggerCache = new TaggerCache()
}
