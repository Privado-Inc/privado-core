package ai.privado.languageEngine.java.passes.config

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.model.{ConfigAndRules, Language, SystemConfig}
import ai.privado.passes.PropertyParserPass
import ai.privado.semantic.language.*
import better.files.File
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class PropertyPassDirSizeFilterTest extends PropertiesFilePassFilterTestBase {
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

  "Test Dir File Filtering" should {
    "Test property file filtering" in {
      val propertyNodes = cpg.property.l
      propertyNodes.size shouldBe 1
      propertyNodes.head.name should equal("dev.key4")
      propertyNodes.head.file.head.name.contains("pro4.yaml") shouldBe true
    }
  }
}

class PropertyPassSizeFilterTest extends PropertiesFilePassFilterTestBase {
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

  "Test file size filtering" should {
    "Test property file filtering" in {
      val propertyNode = cpg.property.l
      propertyNode.size shouldBe 10
      propertyNode.name(".*(prod).*").size shouldBe 0
    }
  }
}

abstract class PropertiesFilePassFilterTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val configFileContents: Map[String, String]
  var inputDir: File   = _
  var outputFile: File = _
  val ruleCache        = new RuleCache()

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

    new PropertyParserPass(cpg, inputDir.toString(), ruleCache, Language.JAVA).createAndApply()

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
