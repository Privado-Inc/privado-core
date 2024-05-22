package ai.privado.languageEngine.java.audit
import ai.privado.audit.DataElementDiscovery
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.tagger.collection.CollectionTagger
import ai.privado.languageEngine.java.tagger.source.*
import io.shiftleft.codepropertygraph.generated.nodes.Member

import scala.collection.mutable
import scala.util.Try

class DataElementDiscoveryTest extends DataElementDiscoveryTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    SourceTagger.runTagger(cpg, ruleCache, taggerCache)
    new CollectionTagger(cpg, ruleCache).createAndApply()
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
    testClassMap.put("AdminDao", AuditTestClassData.adminDao)
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
      classNameList should not contain ("com.test.privado.Dao.AdminDao")
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
      discoveryList should not contain ("com.test.privado.Dao.AdminDao")
    }

    "Test class member variable" in {
      val classList = List("com.test.privado.Entity.User", "com.test.privado.Entity.Account")

      val memberMap = DataElementDiscovery.getMemberUsingClassName(Try(cpg), classList.toSet)

      val classMemberMap = new mutable.HashMap[String, List[Member]]()

      memberMap.foreach { case (key, value) =>
        classMemberMap.put(key.fullName, value)
      }

      classMemberMap.keys.toList should contain("com.test.privado.Entity.User")
      classMemberMap("com.test.privado.Entity.User").size shouldBe 1
      classMemberMap("com.test.privado.Entity.User").head.name should equal("firstName")

      classMemberMap.keys.toList should contain("com.test.privado.Entity.Account")
      classMemberMap("com.test.privado.Entity.Account").size shouldBe 1
      classMemberMap("com.test.privado.Entity.Account").head.name should equal("accountNo")
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
      collectionList should not contain ("com.test.privado.Dao.AdminDao")
    }

    "Test final discovery result" in {
      val classNameList                  = new mutable.HashSet[String]()
      val fileScoreList                  = new mutable.HashSet[String]()
      val memberList                     = new mutable.HashSet[String]()
      val sourceRuleIdMap                = new mutable.HashMap[String, String]()
      val collectionTagMap               = new mutable.HashMap[String, String]()
      val endpointMap                    = new mutable.HashMap[String, String]()
      val methodNameMap                  = new mutable.HashMap[String, String]()
      val memberLineNumberAndTypeMapping = mutable.HashMap[String, (String, String)]()
      val uniqueIdentifiers              = mutable.ArrayBuffer[String]()
      val workbookList                   = DataElementDiscovery.processDataElementDiscovery(Try(cpg), taggerCache)

      workbookList.foreach(row => {
        classNameList += row.head
        fileScoreList += row(2)
        memberList += row(3)
        sourceRuleIdMap.put(row(3), row(6))
        if (!collectionTagMap.contains(row.head)) collectionTagMap.put(row.head, row(7))
        if (!endpointMap.contains(row.head)) endpointMap.put(row.head, row(8))
        if (!methodNameMap.contains(row.head)) methodNameMap.put(row.head, row(9))
        // Bind entity's name to its line number for testing.
        memberLineNumberAndTypeMapping += (row(3) -> (row(10), row.last))
//        memberLineNumberAndTypeMapping += s"${row(3)}+${row(10)}+${row.last}"
        uniqueIdentifiers += row(11)
      })

      memberLineNumberAndTypeMapping("firstName") shouldBe (/* line number */ "5", "Member")
      memberLineNumberAndTypeMapping("accountNo") shouldBe (/* line number */ "5", "Member")
      memberLineNumberAndTypeMapping("invoiceNo") shouldBe (/* line number */ "6", "Member")
      memberLineNumberAndTypeMapping("payment") shouldBe (/* line number */ "11", "Member")
      memberLineNumberAndTypeMapping.contains("nonExistentField") shouldBe false

      // All identifiers generated via MD5 should be unique
      uniqueIdentifiers.size shouldBe uniqueIdentifiers.distinct.size

      // Validate class name in result
      classNameList should contain("com.test.privado.Entity.User")
      classNameList should contain("com.test.privado.Entity.Account")
      classNameList should contain("com.test.privado.Entity.Salary")
      classNameList should contain("com.test.privado.Entity.Invoice")
      classNameList should contain("com.test.privado.Entity.Address")
      classNameList should not contain ("com.test.privado.Controller.UserController")
      classNameList should not contain ("com.test.privado.Controller.AddressController")
      classNameList should not contain ("com.test.privado.Dao.AdminDao")

      // Validate class member in result
      memberList should contain("houseNo")
      memberList should contain("firstName")
      memberList should contain("invoiceNo")
      memberList should contain("payment")
      memberList should contain("accountNo")
      memberList should not contain ("addressInfo")

      fileScoreList should contain("2.0")
      fileScoreList should contain("0.0")

      // validate source Rule ID in result
      sourceRuleIdMap("firstName").toString should equal("Data.Sensitive.FirstName")

      // validate collection Tag in result
      collectionTagMap("com.test.privado.Entity.User").toString should equal("YES")

      // validate collection endpoint in result
      endpointMap("com.test.privado.Entity.User").toString should equal("/user/add")

      // validate collection method name in result
      methodNameMap("com.test.privado.Entity.User").toString should equal(
        "public String userHandler(@RequestBody User user)"
      )
    }

    "Test file score " in {
      var score = DataElementDiscovery.getFileScore("User.java", Try(cpg))
      score shouldBe "2.0"

      score = DataElementDiscovery.getFileScore("Salary.java", Try(cpg))
      score shouldBe "0.0"

    }

    "filter the class having no member" in {
      val classList = List("com.test.privado.Controller.UserController", "com.test.privado.Entity.Address")
      val memberMap = DataElementDiscovery.getMemberUsingClassName(Try(cpg), classList.toSet)

      memberMap.size shouldBe 1
      memberMap.headOption.get._1.fullName should equal("com.test.privado.Entity.Address")
    }
  }
}
