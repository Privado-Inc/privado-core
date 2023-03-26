package ai.privado.languageEngine.java.audit
import ai.privado.audit.DataElementDiscovery
import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.semantic.Language.tagger
import ai.privado.languageEngine.java.tagger.collection.CollectionTagger
import ai.privado.languageEngine.java.tagger.sink.JavaAPITagger
import ai.privado.languageEngine.java.tagger.source.IdentifierTagger
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.tagger.source.LiteralTagger
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

class DataElementDiscoveryTest extends DataElementDiscoveryTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, taggerCache).createAndApply()
    new CollectionTagger(cpg, RuleCache.getRule.sources).createAndApply()
  }

  override val javaFileContentMap: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put("User", AuditTestClassData.user)
    testClassMap.put("Account", AuditTestClassData.account)
    testClassMap.put("Address", AuditTestClassData.address)
    testClassMap.put("UserController", AuditTestClassData.userController)
    testClassMap.put("Salary", AuditTestClassData.salaryLombok)
    testClassMap.put("AddressController", AuditTestClassData.addressController)
    testClassMap.put("Invoice", AuditTestClassData.invoice)
    testClassMap.toMap
  }

  "DataElementDiscovery" should {
    "Test discovery of class name in codebase" in {
      val classNameList = DataElementDiscovery.getSourceUsingRules(Try(cpg))

      classNameList.size shouldBe 4
      classNameList should contain("com.test.privado.Entity.User")
      classNameList should contain("com.test.privado.Entity.Account")
      classNameList should contain("com.test.privado.Entity.Salary")
      classNameList should contain("com.test.privado.Entity.Invoice")
      classNameList should not contain ("com.test.privado.Controller.AddressController")
      classNameList should not contain ("com.test.privado.Controller.UserController")
      classNameList should not contain ("com.test.privado.Entity.Address")
    }

    "Test discovery of class Name in package from class name" in {
      val classList = List("com.test.privado.Entity.User", "com.test.privado.Entity.Account")

      val discoveryList = DataElementDiscovery.extractClassFromPackage(Try(cpg), classList.toSet)
      discoveryList.size shouldBe 5
      discoveryList should contain("com.test.privado.Entity.User")
      discoveryList should contain("com.test.privado.Entity.Account")
      discoveryList should contain("com.test.privado.Entity.Salary")
      discoveryList should contain("com.test.privado.Entity.Invoice")
      discoveryList should contain("com.test.privado.Entity.Address")
      discoveryList should not contain ("com.test.privado.Controller.AddressController")
      discoveryList should not contain ("com.test.privado.Controller.UserController")
    }

    "Test class member variable" in {
      val classList = List("com.test.privado.Entity.User", "com.test.privado.Entity.Account")

      val memberMap = DataElementDiscovery.getMemberUsingClassName(Try(cpg), classList.toSet)

      memberMap(classList.head).size shouldBe 1
      memberMap(classList.head).head.name shouldBe ("firstName")
      memberMap(classList(1)).size shouldBe 1
      memberMap(classList(1)).head.name shouldBe ("accountNo")
    }

    "Test Collection discovery" in {
      val collectionList = DataElementDiscovery.getCollectionInputList(Try(cpg))
      collectionList should contain("com.test.privado.Entity.User")
      collectionList should not contain ("com.test.privado.Entity.Account")
      collectionList should not contain ("com.test.privado.Entity.Salary")
      collectionList should not contain ("com.test.privado.Entity.Invoice")
      collectionList should not contain ("com.test.privado.Entity.Address")
      collectionList should not contain ("com.test.privado.Controller.AddressController")
      collectionList should not contain ("com.test.privado.Controller.UserController")
    }

    "Test final discovery result" in {
      val classNameList    = new ListBuffer[String]()
      val memberList       = new ListBuffer[String]()
      val sourceRuleIdMap  = new mutable.HashMap[String, String]()
      val collectionTagMap = new mutable.HashMap[String, String]()

      val workbookList = DataElementDiscovery.processDataElementDiscovery(Try(cpg), taggerCache)

      workbookList.foreach(row => {
        println(row)
        classNameList += row.head
        memberList += row(1)
        sourceRuleIdMap.put(row(1), row(3))
        collectionTagMap.put(row.head, row(4))
      })

      // Validate class name in result
      classNameList should contain("com.test.privado.Entity.User")
      classNameList should contain("com.test.privado.Entity.Account")
      classNameList should contain("com.test.privado.Entity.Salary")
      classNameList should contain("com.test.privado.Entity.Invoice")
      classNameList should contain("com.test.privado.Entity.Address")
      classNameList should not contain ("com.test.privado.Controller.UserController")
      classNameList should not contain ("com.test.privado.Controller.AddressController")

      // Validate class member in result
      memberList should contain("houseNo")
      memberList should contain("firstName")
      memberList should contain("invoiceNo")
      memberList should contain("payment")
      memberList should contain("accountNo")
      memberList should not contain ("addressInfo")

      // validate sourceRuleID in result
      sourceRuleIdMap("firstName").toString should equal("Data.Sensitive.FirstName")

      // validate collection Tag in result
      collectionTagMap("com.test.privado.Entity.User").toString should equal("YES")
    }
  }
}
