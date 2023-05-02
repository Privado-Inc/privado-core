package ai.privado.languageEngine.javascript.passes.config

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.language._
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.passes.ImportsPass
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language._

import java.nio.file.Paths

class GetEnvironmentTest extends JSPropertiesFilePassTestBase(".env") {

  val db_url = "https://mydb.dbname.in/secret"

  override val configFileContents: String =
    s"""
      |DB_URL=https://mydb.dbname.in/secret
      |DB_NAME=mydb
    """.stripMargin
  override val codeFileContents: String =
    """
      |const dbUrl = process.env['DB_URL']
      |const dbName = process.env.DB_NAME
      |""".stripMargin

  "JS Config File pass should" should {
    "create a file node for the property file" in {
      val files = cpg.file.name.l
      files.filter(_.endsWith(".env")).head.endsWith("/test.env") shouldBe true
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("DB_URL")
        .contains(db_url) shouldBe true
    }

    "connect property nodes to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename.endsWith("/test.env") shouldBe true
    }

    "process another way to connect literals" in {
      val lit = cpg.property.usedAt.l
      lit.exists(node => node.code.matches(".*DB_URL.*")) shouldBe true
    }

    "connect property node to literal via `IS_USED_AT` edge" in {
      val lit = cpg.property.usedAt.l
      lit.exists(node => node.code.matches(".*DB_NAME.*")) shouldBe true
    }
    "connect literal node to property via `ORIGINAL_PROPERTY` edge" in {
      val javaP = cpg.property.usedAt.originalProperty.l.head
      javaP.value shouldBe db_url

      val lit = cpg.property.usedAt.l.head
      lit.originalProperty.head.value shouldBe db_url
      lit.originalPropertyValue.head shouldBe db_url
    }
  }

}

abstract class JSPropertiesFilePassTestBase(fileExtension: String)
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll {

  var cpg: Cpg = _
  val configFileContents: String
  val codeFileContents: String
  var inputDir: File   = _
  var outputFile: File = _

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / s"test$fileExtension").write(configFileContents)

    (inputDir / "unrelated.file").write("foo")
    outputFile = File.newTemporaryFile()

    (inputDir / "GeneralConfig.js").write(codeFileContents)

    val cpgconfig =
      Config(inputPath = inputDir.toString, outputPath = outputFile.toString)
    cpg = new JsSrc2Cpg().createCpgWithAllOverlays(cpgconfig).get

    X2Cpg.applyDefaultOverlays(cpg)
    new ImportsPass(cpg).createAndApply()
    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))

    new PropertiesFilePass(cpg, inputDir.toString, new RuleCache).createAndApply()

  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

}
