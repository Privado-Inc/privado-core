package ai.privado.languageEngine.java.audit

import ai.privado.audit.{AuditReportConstants, DependencyReport}
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.config.ModuleFilePass
import ai.privado.languageEngine.java.passes.module.{DependenciesNodePass, DependencyCategoryPass}
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency

import scala.collection.mutable
import scala.util.Try

class DependencyReportTest extends DependencyReportTestBase {

  override val javaFileContentMap: Map[String, String] = getContent()
  var dependencies: Set[ModuleDependency]              = Set.empty

  override def beforeAll(): Unit = {
    super.beforeAll()
    val moduleCache: ModuleCache = new ModuleCache()
    new ModuleFilePass(cpg, inputDir.toString(), moduleCache, ruleCache).createAndApply()
    new DependenciesNodePass(cpg, moduleCache).createAndApply()
    dependencies = DependencyReport.getDependencyList(Try(cpg))
    new DependencyCategoryPass(cpg, ruleCache, dependencies.toList)
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

      dependencyList.size shouldBe 5

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
    val workbookResult = DependencyReport.processDependencyAudit(Try(cpg), ruleCache, dependencies)

    workbookResult.size shouldBe 6

    workbookResult.foreach(row => {
      dependencySet += row(1)
    })

    dependencySet should contain("org.springframework.spring-core")
    dependencySet should contain("junit.junit")
  }

  "Test Dependency Category" in {
    val dependencyCategoryMap  = new mutable.HashMap[String, String]()
    val dependencyProcessedMap = new mutable.HashMap[String, String]()
    val dependencyRuleMap      = new mutable.HashMap[String, String]()

    val workflowResult = DependencyReport.processDependencyAudit(Try(cpg), ruleCache, dependencies)

    workflowResult.foreach(row => {
      dependencyCategoryMap.put(row(1), row(4))
      dependencyProcessedMap.put(row(1), row(3))
      dependencyRuleMap.put(row(1), row(5))
    })

    // Check Size
    workflowResult.size shouldBe 6

    // Check for Audit Collection Library
    dependencyCategoryMap.contains("org.elasticsearch.client.rest") shouldBe true
    dependencyCategoryMap("org.elasticsearch.client.rest") should equal(
      AuditReportConstants.DEPENDENCY_COLLECTION_LIBRARY_NAME
    )
    dependencyProcessedMap("org.elasticsearch.client.rest") should equal(AuditReportConstants.AUDIT_CHECKED_VALUE)
    dependencyRuleMap("org.elasticsearch.client.rest") should equal("AuditCollection.SpringWebMVC")

    // Check for Audit Web Client Library
    dependencyCategoryMap.contains("org.http4k.http4k-connect-core") shouldBe true
    dependencyCategoryMap("org.http4k.http4k-connect-core") should equal(
      AuditReportConstants.DEPENDENCY_WEB_CLIENT_LIBRARY_NAME
    )
    dependencyProcessedMap("org.http4k.http4k-connect-core") should equal(AuditReportConstants.AUDIT_CHECKED_VALUE)
    dependencyRuleMap("org.http4k.http4k-connect-core") should equal("AuditWebClient.http4k")

    // Check for Audit Utility Library
    dependencyCategoryMap.contains("com.github.scala-incubator.io.scala-io-file_2.11") shouldBe true
    dependencyCategoryMap("com.github.scala-incubator.io.scala-io-file_2.11") should equal(
      AuditReportConstants.DEPENDENCY_UTILITY_LIBRARY_NAME
    )
    dependencyProcessedMap("com.github.scala-incubator.io.scala-io-file_2.11") should equal(
      AuditReportConstants.AUDIT_NOT_CHECKED_VALUE
    )
    dependencyRuleMap("com.github.scala-incubator.io.scala-io-file_2.11") should equal("AuditUtility.Github")

    // Check for Unknown Library
    dependencyCategoryMap.contains("org.springframework.spring-core") shouldBe true
    dependencyCategoryMap("org.springframework.spring-core") should equal(
      AuditReportConstants.DEPENDENCY_UNKNOWN_LIBRARY_NAME
    )
    dependencyProcessedMap("org.springframework.spring-core") should equal(AuditReportConstants.AUDIT_NOT_CHECKED_VALUE)
    dependencyRuleMap("org.springframework.spring-core") should equal(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE)
  }
}
