package ai.privado.languageEngine.java.audit
import ai.privado.audit.auditProcessor
import ai.privado.cache.TaggerCache
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import scala.collection.mutable
import scala.util.Try

class DataElementGetterSetterDiscoveryTest extends AuditTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
  }

  override val javaFileContentMap: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val map = mutable.Map[String, String]()

    map.put("User", AuditTestClassData.user)
    map.put("Account", AuditTestClassData.account)
    map.put("Address", AuditTestClassData.address)
    map.toMap
  }

  "DataElementDiscovery" should {
    "Test full name of Class found" in {
      val list = auditProcessor.getSourceUsingRules(Try(cpg))

      list.size shouldBe 2
      list should contain ("com.ai.privado.Entity.User")
      list should contain ("com.ai.privado.Entity.Account")
      list should not contain ("com.ai.privado.Entity.Address")
    }

    "Test discovery class from package" in {
      val classList = List("com.ai.privado.Entity.User", "com.ai.privado.Entity.Account")

      val discoveryList = auditProcessor.extractClassFromPackage(Try(cpg), classList.toSet)

      discoveryList.size shouldBe 3
      discoveryList should contain ("com.ai.privado.Entity.Address")
    }

    "Test class member variable" in {
      val classList = List("com.ai.privado.Entity.User", "com.ai.privado.Entity.Account")

      val memberMap = auditProcessor.getMemberUsingClassName(Try(cpg), classList.toSet)

      memberMap(classList.head).size shouldBe 1
      memberMap(classList.head).head.name shouldBe ("firstName")
    }

    "Test final discovery result" in {
      val collectionList = auditProcessor.processDataElementDiscovery(Try(cpg), new TaggerCache())

      collectionList.size shouldBe 7
    }
  }
}
