package ai.privado.languageEngine.go.audit

import ai.privado.audit.{DataElementDiscovery, DataElementDiscoveryJS}
import ai.privado.languageEngine.go.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.go.tagger.collection.CollectionTagger
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import io.shiftleft.codepropertygraph.generated.nodes.Member

import scala.collection.mutable
import scala.util.Try

class DataElementDiscoveryTest extends DataElementDiscoveryTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContentMap: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put("User", AuditTestClassData.user)
    testClassMap.put("Account", AuditTestClassData.account)
    testClassMap.put("Address", AuditTestClassData.address)
    testClassMap.toMap
  }

  "DataElementDiscovery" should {
    "Test discovery of class name in codebase" in {
      val classNameList = DataElementDiscoveryJS.getSourceUsingRules(Try(cpg))

      classNameList.size shouldBe 4
      classNameList should contain("entity.User")
      classNameList should contain("entity.Account")
      classNameList should contain("entity.Address")
      classNameList should not contain ("nonExistent.Type")
    }

    "Test discovery of class Name in package from class name" in {
      val classList = List("entity.User", "entity.Account")

      val discoveryList = DataElementDiscovery.extractClassFromPackage(Try(cpg), classList.toSet)
      discoveryList.size shouldBe 4
      discoveryList should contain("entity.User")
      discoveryList should contain("entity.Account")
    }

    "Test class member variable" in {
      val classList = List("entity.User", "entity.Account")

      val memberMap = DataElementDiscovery.getMemberUsingClassName(Try(cpg), classList.toSet)

      val classMemberMap = new mutable.HashMap[String, List[Member]]()

      memberMap.foreach { case (key, value) =>
        classMemberMap.put(key.fullName, value)
      }

      classMemberMap.keys.toList should contain("entity.User")
      classMemberMap("entity.User").size shouldBe 1
      classMemberMap("entity.User").head.name should equal("firstName")

      classMemberMap.keys.toList should contain("entity.Account")
      classMemberMap("entity.Account").size shouldBe 1
      classMemberMap("entity.Account").head.name should equal("accountNo")
    }

    "Test final discovery result" in {
      val classNameList    = new mutable.HashSet[String]()
      val fileScoreList    = new mutable.HashSet[String]()
      val memberList       = new mutable.HashSet[String]()
      val sourceRuleIdMap  = new mutable.HashMap[String, String]()
      val collectionTagMap = new mutable.HashMap[String, String]()
      val endpointMap      = new mutable.HashMap[String, String]()
      val methodNameMap    = new mutable.HashMap[String, String]()

      val workbookList = DataElementDiscoveryJS.processDataElementDiscovery(Try(cpg), taggerCache)

      workbookList.foreach(row => {
        classNameList += row.head
        fileScoreList += row(2)
        memberList += row(3)

        // Prevent overwriting last sane value.
        if (row(6) != "--") {
          sourceRuleIdMap.put(row(3), row(6))
        }

        if (!collectionTagMap.contains(row.head)) collectionTagMap.put(row.head, row(7))
        if (!endpointMap.contains(row.head)) endpointMap.put(row.head, row(8))
        if (!methodNameMap.contains(row.head)) methodNameMap.put(row.head, row(9))
      })

      // Validate class name in result
      classNameList should contain("entity.User")
      classNameList should contain("entity.Account")
      classNameList should contain("entity.Address")

      classNameList should not contain ("nonExistent.Type")

      // Validate class member in result
      memberList should contain("firstName")
      memberList should contain("accountNo")
      memberList should contain("houseNo")
      memberList should not contain ("nonExistentMember")

      fileScoreList should contain("1.5")

      // validate source Rule ID in result
      sourceRuleIdMap("firstName") should equal("Data.Sensitive.FirstName")
    }

    "Test file score " in {
      DataElementDiscoveryJS.getFileScoreJS("User.go", Try(cpg)) shouldBe "1.5"
    }

    "filter the class having no member" in {
      val classList = List("nonExistent.Type", "entity.Address")
      val memberMap = DataElementDiscovery.getMemberUsingClassName(Try(cpg), classList.toSet)

      memberMap.size shouldBe 1
      memberMap.headOption.get._1.fullName should equal("entity.Address")
    }
  }
}
