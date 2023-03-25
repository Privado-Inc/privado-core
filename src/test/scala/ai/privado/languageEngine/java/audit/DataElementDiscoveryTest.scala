package ai.privado.languageEngine.java.audit
import ai.privado.audit.auditProcessor
import ai.privado.cache.TaggerCache
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.tagger.collection.CollectionTagger
import ai.privado.languageEngine.java.tagger.source.IdentifierTagger

import scala.collection.mutable
import scala.util.Try

class DataElementGetterSetterDiscoveryTest extends AuditTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, taggerCache).createAndApply()
    new CollectionTagger(cpg, sourceRule).createAndApply()
  }

  override val pomFileContent =
    """
      |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      |	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
      |	<modelVersion>4.0.0</modelVersion>
      |	<groupId>com.ai.privado</groupId>
      |	<artifactId>lombok-test</artifactId>
      |	<version>0.0.1-SNAPSHOT</version>
      |
      |	<dependencies>
      |		<!-- https://mvnrepository.com/artifact/org.projectlombok/lombok -->
      |		<dependency>
      |			<groupId>org.projectlombok</groupId>
      |			<artifactId>lombok</artifactId>
      |			<version>1.18.4</version>
      |			<scope>provided</scope>
      |		</dependency>
      |
      |		<dependency>
      |			<groupId>org.slf4j</groupId>
      |			<artifactId>slf4j-api</artifactId>
      |			<version>1.7.25</version>
      |		</dependency>
      |	</dependencies>
      |
      |</project>""".stripMargin

  override val javaFileContentMap: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val map = mutable.Map[String, String]()

    map.put("User", AuditTestClassData.user)
    map.put("Account", AuditTestClassData.account)
    map.put("Address", AuditTestClassData.address)
    map.put("UserController", AuditTestClassData.userController)
    map.put("Salary", AuditTestClassData.salaryLombok)
    map.put("AddressController", AuditTestClassData.addressController)
    map.toMap
  }

  "DataElementDiscovery" should {
    "Test full name of Class found" in {
      val list = auditProcessor.getSourceUsingRules(Try(cpg))

      list.size shouldBe 2
      list should contain("com.ai.privado.Entity.User")
      list should contain("com.ai.privado.Entity.Account")
      list should not contain ("com.ai.privado.Entity.Address")
      list should contain("com.ai.privado.Entity.Salary")
      list should not contain("com.ai.privado.Controller.AddressController")
      list should not contain("com.ai.privado.Controller.UserController")
    }

    "Test discovery class from package" in {
      val classList = List("com.ai.privado.Entity.User", "com.ai.privado.Entity.Account")

      val discoveryList = auditProcessor.extractClassFromPackage(Try(cpg), classList.toSet)

      discoveryList.size shouldBe 4
      discoveryList should contain("com.ai.privado.Entity.Address")
    }

    "Test class member variable" in {
      val classList = List("com.ai.privado.Entity.User", "com.ai.privado.Entity.Account")

      val memberMap = auditProcessor.getMemberUsingClassName(Try(cpg), classList.toSet)

      memberMap(classList.head).size shouldBe 1
      memberMap(classList.head).head.name shouldBe ("firstName")
    }

    "Test Collection discovery" in {
      val collectionList = auditProcessor.getCollectionInputList(Try(cpg))

      println("rrrr")
      println(collectionList)
    }

    "Test final discovery result" in {
      val collectionList = auditProcessor.processDataElementDiscovery(Try(cpg), new TaggerCache())

      println(collectionList)

      collectionList.size shouldBe 9
    }
  }
}
