package ai.privado.languageEngine.go.audit

import ai.privado.audit.{DataElementDiscovery, DataElementDiscoveryUtils}
import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.go.audit.TestData.AuditTestClassData
import ai.privado.model.*
import ai.privado.testfixtures.GoFrontendTestSuite
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Member

import scala.collection.mutable
import scala.util.Try

class DataElementDiscoveryTest extends GoFrontendTestSuite {

  private val taggerCache = new TaggerCache()

  val sourceRule: List[RuleInfo] = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*firstName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.GO,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, List(), List(), List(), List(), List(), List(), List(), List(), List())

  val ruleCache = new RuleCache()
  ruleCache.setRule(rule)

  private val cpg: Cpg = code(AuditTestClassData.user, "User.go")
    .moreCode(AuditTestClassData.account, "Account.go")
    .moreCode(AuditTestClassData.address, "Address.go")
    .moreCode(AuditTestClassData.userCreation, "UserCreation.go")
    .withRuleCache(ruleCache)

  "DataElementDiscovery" should {
    "Test discovery of class name in codebase" in {
      val classNameList = DataElementDiscoveryUtils.getSourceUsingRules(Try(cpg))

      classNameList.size shouldBe 5
      classNameList should contain("entity.User")
      classNameList should contain("entity.Account")
      classNameList should contain("entity.Address")
      classNameList should contain("UserTableCreation")
      classNameList should not contain ("nonExistent.Type")
    }

    "Test class member variable" in {
      val classList = List("entity.User", "entity.Account")

      val memberMap = DataElementDiscoveryUtils.getMemberUsingClassName(Try(cpg), classList.toSet, Language.GO)

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
      val classNameList                  = new mutable.HashSet[String]()
      val fileScoreList                  = new mutable.HashSet[String]()
      val memberList                     = new mutable.HashSet[String]()
      val sourceRuleIdMap                = new mutable.HashMap[String, String]()
      val collectionTagMap               = new mutable.HashMap[String, String]()
      val endpointMap                    = new mutable.HashMap[String, String]()
      val methodNameMap                  = new mutable.HashMap[String, String]()
      val memberLineNumberAndTypeMapping = mutable.HashMap[String, (String, String)]()
      val workbookList = DataElementDiscovery.processDataElementDiscovery(Try(cpg), taggerCache, Language.GO)

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

      memberLineNumberAndTypeMapping("firstName") shouldBe (/* line number */ "5", "Member")
      memberLineNumberAndTypeMapping("fName") shouldBe (/* line number */ "13", "Identifier")
      memberLineNumberAndTypeMapping("accountNo") shouldBe (/* line number */ "5", "Member")
      memberLineNumberAndTypeMapping("accNo") shouldBe (/* line number */ "9", "Identifier")
      memberLineNumberAndTypeMapping("houseNo") shouldBe (/* line number */ "5", "Member")
      memberLineNumberAndTypeMapping.contains("nonExistentField") shouldBe false

      memberLineNumberAndTypeMapping("tb_firstName") shouldBe (/* line number */ "2", "SqlNode")
      memberLineNumberAndTypeMapping("tb_lastName") shouldBe (/* line number */ "3", "SqlNode")
      memberLineNumberAndTypeMapping("tb_emailId") shouldBe (/* line number */ "4", "SqlNode")
      memberLineNumberAndTypeMapping("tb_password") shouldBe (/* line number */ "5", "SqlNode")

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

      // validate source Rule ID in result
      sourceRuleIdMap("firstName") should equal("Data.Sensitive.FirstName")
    }

    "Test file score " in {
      DataElementDiscovery.getFileScoreJS("User.go", Try(cpg)) shouldBe "1.5"
    }

    "filter the class having no member" in {
      val classList = List("nonExistent.Type", "entity.Address")
      val memberMap = DataElementDiscoveryUtils.getMemberUsingClassName(Try(cpg), classList.toSet, Language.GO)

      memberMap.size shouldBe 1
      memberMap.headOption.get._1.fullName should equal("entity.Address")
    }
  }
}
