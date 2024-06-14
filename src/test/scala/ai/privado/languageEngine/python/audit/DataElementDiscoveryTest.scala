package ai.privado.languageEngine.python.audit

import ai.privado.audit.{DataElementDiscovery, DataElementDiscoveryUtils}
import ai.privado.languageEngine.python.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.python.tagger.collection.CollectionTagger
import ai.privado.languageEngine.python.tagger.source.IdentifierTagger
import io.shiftleft.codepropertygraph.generated.nodes.Member
import ai.privado.model.Language

import scala.collection.mutable
import scala.util.Try

class DataElementDiscoveryTest extends DataElementDiscoveryTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val pyFileContentMap: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put("User", AuditTestClassData.user)
    testClassMap.put("Account", AuditTestClassData.account)
    testClassMap.put("Address", AuditTestClassData.address)
    testClassMap.toMap
  }

  "DataElementDiscovery" should {
    "Test class member variable" in {
      val classList = List("User.py:<module>.User", "Account.py:<module>.Account")

      val memberMap = DataElementDiscoveryUtils.getMemberUsingClassName(Try(cpg), classList.toSet, Language.PYTHON)

      val classMemberMap = new mutable.HashMap[String, List[Member]]()

      memberMap.foreach { case (key, value) =>
        classMemberMap.put(key.fullName, value)
      }

      classMemberMap.keys.toList should contain("User.py:<module>.User")

      // __init__, field, getter, setter
      classMemberMap("User.py:<module>.User").size shouldBe 1
      classMemberMap("User.py:<module>.User").last.name should equal("firstName")

      classMemberMap.keys.toList should contain("Account.py:<module>.Account")

      // __init__, field, setter
      classMemberMap("Account.py:<module>.Account").size shouldBe 1
      classMemberMap("Account.py:<module>.Account").last.name should equal("accountNo")
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
      val workbookList = DataElementDiscovery.processDataElementDiscovery(Try(cpg), taggerCache, Language.PYTHON)

      workbookList.foreach(row => {
        classNameList += row.head
        fileScoreList += row(2)
        memberList += row(3)

        memberLineNumberAndTypeMapping += (row(3) -> (row(10), row.last))

        // Prevent overwriting last sane value.
        if (row(6) != "--") {
          sourceRuleIdMap.put(row(3), row(6))
        }

        if (!collectionTagMap.contains(row.head)) collectionTagMap.put(row.head, row(7))
        if (!endpointMap.contains(row.head)) endpointMap.put(row.head, row(8))
        if (!methodNameMap.contains(row.head)) methodNameMap.put(row.head, row(9))
      })

      memberLineNumberAndTypeMapping("firstName") shouldBe (/* line number */ "4", "FieldIdentifier")
      memberLineNumberAndTypeMapping("accountNo") shouldBe (/* line number */ "4", "FieldIdentifier")
      memberLineNumberAndTypeMapping("fName") shouldBe (/* line number */ "3", "Method Parameter")
      memberLineNumberAndTypeMapping("houseNo") shouldBe (/* line number */ "4", "FieldIdentifier")
      memberLineNumberAndTypeMapping("hNo") shouldBe (/* line number */ "3", "Method Parameter")
      memberLineNumberAndTypeMapping.contains("nonExistentField") shouldBe false

      // Validate class name in result
      classNameList should contain("User.py:<module>.User")
      classNameList should contain("Account.py:<module>.Account")
      classNameList should contain("Address.py:<module>.Address")
      classNameList should not contain ("NonExistent.py:<module>.NonExistent")

      // Validate class member in result
      memberList should contain("firstName")
      memberList should contain("accountNo")
      memberList should contain("houseNo")
      memberList should not contain ("nonExistentMember")

      // validate source Rule ID in result
      sourceRuleIdMap("firstName") should equal("Data.Sensitive.FirstName")
    }

    "filter the class having no member" in {
      val classList = List("NonExistent.py:<module>.NonExistent", "Address.py:<module>.Address")
      val memberMap = DataElementDiscoveryUtils.getMemberUsingClassName(Try(cpg), classList.toSet, Language.PYTHON)

      memberMap.size shouldBe 1
      memberMap.headOption.get._1.fullName should equal("Address.py:<module>.Address")
    }
  }
}
