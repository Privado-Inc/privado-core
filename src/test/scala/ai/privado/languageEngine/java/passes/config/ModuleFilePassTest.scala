package ai.privado.languageEngine.java.passes.config

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.language.module.{NodeStarters, StepsForDependency, StepsForModule}
import ai.privado.languageEngine.java.passes.module.DependenciesNodePass
import ai.privado.model._
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Module, ModuleDependency}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ModuleMavenTest extends ModuleFilePassTestBase {
  override val moduleFileMap: Map[String, String] = getContent()

  override def beforeAll() = {
    super.beforeAll()
    val moduleCache: ModuleCache = new ModuleCache()
    new ModuleFilePass(cpg, inputDir.toString(), moduleCache, ruleCache).createAndApply()
    new DependenciesNodePass(cpg, moduleCache).createAndApply()
  }

  private def getContent(): Map[String, String] = {
    val testModuleMap = mutable.HashMap[String, String]()

    val parentPom =
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

    val firstChildPom =
      """
        |<project xmlns="http://maven.apache.org/POM/4.0.0"
        |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
        |         http://maven.apache.org/maven-v4_0_0.xsd">
        |
        |  <modelVersion>3.0.0</modelVersion>
        |  <groupId>com.child</groupId>
        |  <artifactId>child-module</artifactId>
        |  <version>2.0.0</version>
        |
        |  <parent>
        |    <groupId>com.example</groupId>
        |    <artifactId>my-java-project</artifactId>
        |    <version>1.0.0</version>
        |  </parent>
        |
        |  <dependencies>
        |    <dependency>
        |      <groupId>org.apache.commons</groupId>
        |      <artifactId>commons-lang3</artifactId>
        |      <version>3.12.0</version>
        |    </dependency>
        |  </dependencies>
        |
        |</project>
        |""".stripMargin

    val secondChildPom =
      """
        <project xmlns="http://maven.apache.org/POM/4.0.0"
        |         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        |         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
        |         http://maven.apache.org/maven-v4_0_0.xsd">
        |
        |  <modelVersion>3.0.0</modelVersion>
        |  <groupId>com.sub-child</groupId>
        |  <artifactId>sub-child-module</artifactId>
        |  <version>2.0.0</version>
        |
        |  <parent>
        |    <groupId>com.child</groupId>
        |    <artifactId>child-module</artifactId>
        |    <version>2.0.0</version>
        |  </parent>
        |
        |  <dependencies>
        |    <dependency>
        |      <groupId>com.ibeetl</groupId>
        |      <artifactId>sql-springboot-starter</artifactId>
        |      <version>3.22.0</version>
        |    </dependency>
        |  </dependencies>
        |
        |</project>
        |""".stripMargin

    testModuleMap.put("project/pom.xml", parentPom)
    testModuleMap.put("project/child-module/pom.xml", firstChildPom)
    testModuleMap.put("project/child-module/child-module/pom.xml", secondChildPom)
    testModuleMap.toMap
  }

  "MavenModuleInfoPass" should {
    "Test pom file Detection" in {
      val pomFiles = cpg.module.file.toList

      pomFiles.size shouldBe 3
    }

    "Test pom.xml content" in {
      val moduleMap = new mutable.HashMap[String, Module]()
      val pomFiles  = cpg.module.toList

      pomFiles.foreach(file => {
        moduleMap.put(file.groupid, file)
      })

      // Test parent POM
      moduleMap.contains("com.example") shouldBe true
      moduleMap("com.example").artifactid shouldBe ("my-java-project")
      moduleMap("com.example").version.getOrElse("") shouldBe ("1.0.0")

      // Test child POM
      moduleMap.contains("com.child") shouldBe true
      moduleMap("com.child").artifactid shouldBe ("child-module")
      moduleMap("com.child").version.getOrElse("") shouldBe ("2.0.0")

      // Test child of child POM
      moduleMap.contains("com.sub-child") shouldBe true
      moduleMap("com.sub-child").artifactid shouldBe ("sub-child-module")
      moduleMap("com.sub-child").version.getOrElse("") shouldBe ("2.0.0")

    }
  }

  "MavenModuleDependencyPass" should {
    "Test Maven Dependency List" in {
      val dependencyGroupIdSet    = new mutable.HashSet[String]()
      val dependencyArtifactIdSet = new mutable.HashSet[String]()
      val dependencyVersionSet    = new mutable.HashSet[String]()
      val dependencySet           = new mutable.HashSet[ModuleDependency]()

      val dependencies = cpg.module.dependencies.toList

      dependencies.foreach(dependency => {
        dependencyGroupIdSet += dependency.groupid
        dependencyArtifactIdSet += dependency.artifactid
        dependencyVersionSet += dependency.version.getOrElse("")
        dependencySet += dependency
      })

      dependencySet.size shouldBe 4

      // Test Dependency groupId
      dependencyGroupIdSet should contain("org.springframework")
      dependencyGroupIdSet should contain("junit")
      dependencyGroupIdSet should contain("org.apache.commons")
      dependencyGroupIdSet should contain("com.ibeetl")

      // Test Dependency artifactId
      dependencyArtifactIdSet should contain("spring-core")
      dependencyArtifactIdSet should contain("junit")
      dependencyArtifactIdSet should contain("commons-lang3")
      dependencyArtifactIdSet should contain("sql-springboot-starter")

      // Test Dependency Versions
      dependencyVersionSet should contain("5.3.9")
      dependencyVersionSet should contain("4.13.2")
      dependencyVersionSet should contain("3.12.0")
      dependencyVersionSet should contain("3.22.0")

    }

    "Test child module list with filter" in {
      val childModule = cpg.module.filter(m => m.groupid matches ("com.child")).toList

      childModule.size shouldBe 1
    }

    "Test child module Dependency list" in {
      val dependencyGroupIdList    = new mutable.HashSet[String]()
      val dependencyArtifactIdList = new mutable.HashSet[String]()
      val dependencyVersionList    = new mutable.HashSet[String]()

      val childModuleDependencies = cpg.module.filter(m => m.groupid matches ("com.child")).dependencies.toList

      childModuleDependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid
        dependencyVersionList += dependency.version.getOrElse("")
      })

      childModuleDependencies.size shouldBe 3

      // Test Dependency groupId
      dependencyGroupIdList should contain("org.springframework")
      dependencyGroupIdList should contain("junit")
      dependencyGroupIdList should contain("org.apache.commons")
      dependencyGroupIdList should not contain ("com.ibeetl")

      // Test Dependency artifactId
      dependencyArtifactIdList should contain("spring-core")
      dependencyArtifactIdList should contain("junit")
      dependencyArtifactIdList should contain("commons-lang3")
      dependencyArtifactIdList should not contain ("sql-springboot-starter")

      // Test Dependency Versions
      dependencyVersionList should contain("5.3.9")
      dependencyVersionList should contain("4.13.2")
      dependencyVersionList should contain("3.12.0")
      dependencyVersionList should not contain ("3.22.0")
    }

    "Test child of child POM Dependency" in {
      val dependencyGroupIdList    = new mutable.HashSet[String]()
      val dependencyArtifactIdList = new mutable.HashSet[String]()
      val dependencyVersionList    = new mutable.HashSet[String]()

      val childModuleDependencies = cpg.module.filter(m => m.groupid matches ("com.sub-child")).dependencies.toList

      childModuleDependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid
        dependencyVersionList += dependency.version.getOrElse("")
      })

      childModuleDependencies.size shouldBe 4

      // Test Dependency groupId
      dependencyGroupIdList should contain("org.springframework")
      dependencyGroupIdList should contain("junit")
      dependencyGroupIdList should contain("org.apache.commons")
      dependencyGroupIdList should contain("com.ibeetl")

      // Test Dependency artifactId
      dependencyArtifactIdList should contain("spring-core")
      dependencyArtifactIdList should contain("junit")
      dependencyArtifactIdList should contain("commons-lang3")
      dependencyArtifactIdList should contain("sql-springboot-starter")

      // Test Dependency Versions
      dependencyVersionList should contain("5.3.9")
      dependencyVersionList should contain("4.13.2")
      dependencyVersionList should contain("3.12.0")
      dependencyVersionList should contain("3.22.0")
    }

    "Test Maven Dependency with filter" in {
      val dependencyGroupIdList    = new mutable.HashSet[String]()
      val dependencyArtifactIdList = new mutable.HashSet[String]()
      val dependencyVersionList    = new mutable.HashSet[String]()

      val filteredDependencies = cpg.module.dependencies.filter(d => d.groupid matches ("org.springframework")).toList

      filteredDependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid
        dependencyVersionList += dependency.version.getOrElse("")
      })

      // because All three module have this dependency
      filteredDependencies.size shouldBe 3

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
      val fileSet  = new mutable.HashSet[io.shiftleft.codepropertygraph.generated.nodes.File]()
      val fileList = cpg.module.dependencies.file.toList

      fileList.foreach(file => {
        fileSet += file
      })

      fileSet.size shouldBe 3
    }
  }
}

class ModuleGradleTest extends ModuleFilePassTestBase {
  override val moduleFileMap: Map[String, String] = getContent()

  override def beforeAll() = {
    super.beforeAll()
    val moduleCache: ModuleCache = new ModuleCache()
    new ModuleFilePass(cpg, inputDir.toString(), moduleCache, ruleCache).createAndApply()
    new DependenciesNodePass(cpg, moduleCache).createAndApply()
  }

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
      val pomFile = cpg.module.file.toList

      pomFile.size shouldBe 1
      pomFile.head.name.contains("build.gradle") shouldBe true
    }

    "Test build.gradle content" in {
      val buildGradleFile = cpg.module.toList

      buildGradleFile.size shouldBe 1
      buildGradleFile.head.groupid shouldBe "com.test"
      buildGradleFile.head.version.getOrElse("") shouldBe "0.0.1-SNAPSHOT"
    }
  }

  "GradleModuleDependencyPass" should {
    "Test Gradle Dependency List" in {
      val dependencyGroupIdList    = new ListBuffer[String]()
      val dependencyArtifactIdList = new ListBuffer[String]()
      val dependencyVersionList    = new ListBuffer[String]()

      val dependencies = cpg.module.dependencies.toList

      dependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid
        dependencyVersionList += dependency.version.getOrElse("")
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

      val filteredDependencies = cpg.module.dependencies.filter(d => d.groupid matches ("com.google.guava")).toList

      filteredDependencies.foreach(dependency => {
        dependencyGroupIdList += dependency.groupid
        dependencyArtifactIdList += dependency.artifactid
        dependencyVersionList += dependency.version.getOrElse("")
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
      val fileList = cpg.module.dependencies.file.toList

      fileList.head.name.contains("build.gradle") shouldBe true
    }
  }

}

abstract class ModuleFilePassTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg        = _
  var inputDir: File  = _
  var outputDir: File = _
  val moduleFileMap: Map[String, String]
  val ruleCache = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "project/child-module/child-module").createDirectoryIfNotExists()
    for ((fileName, content) <- moduleFileMap) {
      (inputDir / fileName).write(content)
    }

    outputDir = File.newTemporaryDirectory()
    val config =
      Config(fetchDependencies = true).withInputPath(inputDir.pathAsString).withOutputPath(outputDir.pathAsString)
    val javaSrc = new JavaSrc2Cpg()
    val xtocpg = javaSrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

    cpg = xtocpg.get
    ruleCache.setRule(rule)

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
    ConfigAndRules(sourceRule, List(), collectionRule, List(), List(), List(), List(), List(), List(), List())

  val taggerCache = new TaggerCache()
}
