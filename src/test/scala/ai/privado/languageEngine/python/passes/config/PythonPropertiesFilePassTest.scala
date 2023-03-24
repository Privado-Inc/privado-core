package ai.privado.languageEngine.python.passes.config

import ai.privado.languageEngine.java.language._
import better.files.File
import io.joern.pysrc2cpg.{Py2CpgOnFileSystem, Py2CpgOnFileSystemConfig}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.nodes.{JavaProperty, Literal}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import java.nio.file.Paths

class GetEnvironmentTest extends PythonPropertiesFilePassTestBase(".env") {

  val mongo_url = "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"
  override val configFileContents: String =
    """
       |MONGO_URL=mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority
       |DEV=False
       |""".stripMargin
  override val codeFileContents: String =
    """
      |import os
      |
      |mongo_url = os.environ.get("MONGO_URL")""".stripMargin

  "ConfigFilePass" should {
    "create a file node for the property file" in {
      val files = cpg.file.name.l
      files.filter(_.endsWith(".env")).head.endsWith("/test.env") shouldBe true
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("MONGO_URL")
        .contains(mongo_url) shouldBe true
    }

    "connect property nodes to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename.endsWith("/test.env") shouldBe true
    }

    "connect property node to literal via `IS_USED_AT` edge" in {
      val List(lit: Literal) = cpg.property.usedAt.l
      lit.code shouldBe "\"MONGO_URL\""
    }
    "connect literal node to property via `ORIGINAL_PROPERTY` edge" in {
      val List(javaP: JavaProperty) = cpg.property.usedAt.originalProperty.l
      javaP.value shouldBe mongo_url

      val List(lit: Literal) = cpg.property.usedAt.l
      lit.originalProperty.head.value shouldBe mongo_url
      lit.originalPropertyValue.head shouldBe mongo_url
    }
  }
}

class INIFileTest extends PythonPropertiesFilePassTestBase(".ini") {
  override val configFileContents: String =
    """
      |[mysql]
      |host = localhost
      |user = user7
      |passwd = s$cret
      |db = ydb""".stripMargin
  override val codeFileContents: String =
    """
      |import configparser
      |
      |config = configparser.ConfigParser()
      |db_host = config['mysql']['host']
      |""".stripMargin

  "create a file node for the property file" in {
    val files = cpg.file.name.l
    files.filter(_.endsWith(".ini")).head.endsWith("/test.ini") shouldBe true
  }

  "create a `property` node for each property" in {
    val properties = cpg.property.map(x => (x.name, x.value)).toMap
    properties
      .get("host")
      .contains("localhost") shouldBe true
  }

  "connect property nodes to file" in {
    val List(filename: String) = cpg.property.file.name.dedup.l
    filename.endsWith("/test.ini") shouldBe true
  }

}
abstract class PythonPropertiesFilePassTestBase(fileExtension: String)
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

    (inputDir / "GeneralConfig.py").write(codeFileContents)
    val pythonConfig = Py2CpgOnFileSystemConfig(Paths.get(outputFile.toString()), Paths.get(inputDir.toString()))
    cpg = new Py2CpgOnFileSystem().createCpg(pythonConfig).get
    new PythonPropertyFilePass(cpg, inputDir.toString()).createAndApply()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

}
