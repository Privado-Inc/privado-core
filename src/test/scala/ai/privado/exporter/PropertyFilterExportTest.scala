package ai.privado.exporter

import ai.privado.cache.{PropertyFilterCache, RuleCache}
import ai.privado.model.{ConfigAndRules, Language, SystemConfig}
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*

import scala.collection.mutable

class PropertyFilterExportBySizeTest extends PropertyFilterExportTestBase {
  override val configFileContents: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val testJsonFiles = mutable.HashMap[String, String]()

    val propertyFileContent1 = new StringBuilder("dev:\n")
    for (i <- 1 to 10) {
      propertyFileContent1.append(s"  key$i: value$i\n")
    }

    val propertyFileContent2 = new StringBuilder("prod:\n")
    for (i <- 1 to 500) {
      propertyFileContent2.append(s"  key$i: value$i\n")
    }

    testJsonFiles.put("project/pro1.yaml", propertyFileContent1.toString())
    testJsonFiles.put("project/pro2.yaml", propertyFileContent2.toString())
    testJsonFiles.toMap
  }

  "Test file size filtering export result" should {
    "Test property file filtering" in {
      val propertyNode = cpg.property.l
      propertyNode.size shouldBe 10
      propertyNode.name(".*(prod).*").size shouldBe 0
    }

    "Test size filtering export data" in {
      val filteredData = propertyFilterCache.getFileSkippedBySizeData(ruleCache)
      filteredData.currentFileSizeLimit should equal("1")

      filteredData.skipLists.size shouldBe 1
      filteredData.skipLists.headOption.get.file.contains("project/pro2.yaml") shouldBe true
      filteredData.skipLists.headOption.get.file.contains("project/pro1.yaml") shouldBe false

    }
  }
}

class PropertyFilterExportByDirCountTest extends PropertyFilterExportTestBase {
  override val configFileContents: Map[String, String] = getContent()

  def getContent(): Map[String, String] = {
    val testJsonFiles = mutable.HashMap[String, String]()

    testJsonFiles.put(
      "project/pro1.yaml",
      """
        |dev:
        | key1: value1
        |""".stripMargin
    )

    testJsonFiles.put(
      "project/pro2.yaml",
      """
        |dev:
        | key2: value2
        |""".stripMargin
    )

    testJsonFiles.put(
      "project/pro3.yaml",
      """
        |dev:
        | key3: value3
        |""".stripMargin
    )

    testJsonFiles.put(
      "project/config/pro4.yaml",
      """
        |dev:
        | key4: value4
        |""".stripMargin
    )

    testJsonFiles.toMap
  }

  "Test Dir file filtering export result" should {
    "Test property file filtering" in {
      val propertyNodes = cpg.property.l
      propertyNodes.size shouldBe 1
      propertyNodes.head.name should equal("dev.key4")
      propertyNodes.head.file.head.name.contains("pro4.yaml") shouldBe true
    }

    "Test dir count filtering export data" in {
      val filteredData = propertyFilterCache.getFileSkippedDirCountData(ruleCache)
      filteredData.skipList.size shouldBe 1
      filteredData.skipList.headOption.get.files.size shouldBe 3
    }
  }
}

abstract class PropertyFilterExportTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  var cpg: Cpg = _
  val configFileContents: Map[String, String]
  var inputDir: File      = _
  var outputFile: File    = _
  val ruleCache           = new RuleCache()
  val propertyFilterCache = new PropertyFilterCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "project/config").createDirectoryIfNotExists()
    for ((key, content) <- configFileContents) {
      (inputDir / key).write(content)
    }
    (inputDir / "javaFile.java").write("")
    outputFile = File.newTemporaryFile()
    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)

    cpg = new JavaSrc2Cpg()
      .createCpg(config)
      .map { cpg =>
        applyDefaultOverlays(cpg)
        cpg
      }
      .get

    ruleCache.setRule(rule)

    new PropertyParserPass(cpg, inputDir.toString(), ruleCache, Language.JAVA, propertyFilterCache).createAndApply()

    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

  val systemConfig = List(
    SystemConfig("propertyFileDirCountLimit", "2", Language.JAVA, "", Array()),
    SystemConfig("propertyFileSizeLimit", "1", Language.JAVA, "", Array())
  )

  val rule: ConfigAndRules =
    ConfigAndRules(systemConfig = systemConfig)
}
