package ai.privado.languageEngine.java.audit

import ai.privado.audit.DependencyReport
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.config.ModuleFilePass
import ai.privado.languageEngine.java.passes.module.DependenciesNodePass

import scala.collection.mutable
import scala.util.Try

class DependencyReportTest extends DependencyReportTestBase {

  override val javaFileContentMap: Map[String, String] = getContent()

  override def beforeAll(): Unit = {
    super.beforeAll()
    val moduleCache: ModuleCache = new ModuleCache()
    new ModuleFilePass(cpg, inputDir.toString(), moduleCache).createAndApply()
    new DependenciesNodePass(cpg, moduleCache).createAndApply()
  }

  def getContent(): Map[String, String] = {
    val testPOMFileMap = mutable.HashMap[String, String]()

    testPOMFileMap.put("project/pom.xml", AuditTestClassData.parentPOMFile)
    testPOMFileMap.toMap
  }

  "Discovery Report" should {
    "Test Dependency List" in {
      val dependencyGroupIdSet = new mutable.HashSet[String]()
      val dependencyList       = DependencyReport.getDependencyList(Try(cpg))

      dependencyList.size shouldBe 2

      dependencyList.foreach(dependency => {
        dependencyGroupIdSet += dependency.groupid
      })

      // Test GroupId
      dependencyGroupIdSet should contain("org.springframework")
      dependencyGroupIdSet should contain("junit")
    }
  }

  "Test final Result" in {
    val dependencySet  = new mutable.HashSet[String]()
    val workbookResult = DependencyReport.processDependencyAudit(Try(cpg))

    workbookResult.size shouldBe 3

    workbookResult.foreach(row => {
      dependencySet += row(1)
    })

    dependencySet should contain("org.springframework.spring-core")
    dependencySet should contain("junit.junit")
  }
}
