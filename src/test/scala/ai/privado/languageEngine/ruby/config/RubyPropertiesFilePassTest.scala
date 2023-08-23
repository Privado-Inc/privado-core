package ai.privado.languageEngine.ruby.config

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.languageEngine.java.language.*
import ai.privado.languageEngine.ruby.passes.config.RubyPropertyLinkerPass
import ai.privado.model.Language
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RubyPropertiesFilePassTest extends RubyPropertiesFilePassTestBase(".env") {

  val mongourl = "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"

  override val configFileContents: String =
    """
      |MONGO_URL=mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority
      |DB_NAME=mydb
      |""".stripMargin

  override val codeFileContents: String =
    """
      |mongo_url = ENV["MONGO_URL"]
      |db_name = ENV.fetch["DB_NAME"]
      |""".stripMargin

  "ConfigFilePass" should {
    "create a file node for config file" in {
      val files = cpg.file.name.l
      files.filter(_.endsWith(".env")).head.endsWith("/config.env") shouldBe true
    }

    "create a 'property' node" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties.get("MONGO_URL").contains(mongourl) shouldBe true
    }

    "connect property node to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename.endsWith("/config.env") shouldBe true
    }

    "process another way to connect literals" in {
      val lit = cpg.property.usedAt.l
      lit.exists(node => node.code.matches(".*DB_NAME.*")) shouldBe true
    }

    "connect property node to literal via `IS_USED_AT` edge" in {
      val lit = cpg.property.usedAt.l.head
      lit.code shouldBe "ENV[\"MONGO_URL\"]"
    }

    "connect literal node to property via `ORIGINAL_PROPERTY` edge" in {
      val javaP = cpg.property.usedAt.originalProperty.l.head
      javaP.value shouldBe mongourl

      val lit = cpg.property.usedAt.l.head
      lit.originalProperty.head.value shouldBe mongourl
      lit.originalPropertyValue.head shouldBe mongourl
    }
  }
}

abstract class RubyPropertiesFilePassTestBase(fileExtension: String)
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll {
  var cpg: Cpg = _
  val configFileContents: String
  val codeFileContents: String
  var inputDir: File  = _
  var outputDir: File = _

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / s"config$fileExtension").write(configFileContents)
    (inputDir / s"code.rb").write(codeFileContents)
    outputDir = File.newTemporaryDirectory()

    val config  = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputDir.pathAsString)
    val rubySrc = new RubySrc2Cpg()
    val xtocpg = rubySrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

    cpg = xtocpg.get
    new PropertyParserPass(cpg, inputDir.pathAsString, new RuleCache(), Language.RUBY).createAndApply()
    new RubyPropertyLinkerPass(cpg).createAndApply()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputDir.delete()
    super.afterAll()
  }
}
