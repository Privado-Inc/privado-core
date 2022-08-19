package ai.privado.passes.config

import better.files.File
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language._
import ai.privado.language._
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, JavaProperty, Literal, MethodParameterIn}

class AnnotationTests extends PropertiesFilePassTestBase {
  override val configFileContents: String =
    """
      |internal.logger.api.base=https://logger.privado.ai/
      |""".stripMargin
  override val javaFileContents: String =
    """
      |
      |import org.springframework.beans.factory.annotation.Value;
      |
      |class Foo {
      |
      |public AuthenticationService(UserRepository userr, SessionsR sesr, ModelMapper mapper,
      |			ObjectMapper objectMapper, @Qualifier("ApiCaller") ExecutorService apiExecutor, SlackStub slackStub,
      |			SendGridStub sgStub, @Value("${internal.logger.api.base}") String loggerBaseURL) {
      |   }
      |}
      |""".stripMargin

  "ConfigFilePass" should {
    "connect annotated parameter to property" in {
      val List(param: MethodParameterIn) = cpg.property.usedAt.l
      param.name shouldBe "loggerBaseURL"
    }

    "connect property to annotated parameter" in {
      val List(javaP: JavaProperty) = cpg.property.usedAt.originalProperty.l
      javaP.value shouldBe "https://logger.privado.ai/"
    }
  }
}

class GetPropertyTests extends PropertiesFilePassTestBase {
  override val configFileContents = """
      |accounts.datasource.url=jdbc:mariadb://localhost:3306/accounts?useSSL=false
      |internal.logger.api.base=https://logger.privado.ai/
      |""".stripMargin
  override val javaFileContents =
    """
      | import org.springframework.core.env.Environment;
      |
      |public class GeneralConfig {
      |   public DataSource dataSource() {
      |     DriverManagerDataSource dataSource = new DriverManagerDataSource();
      |     dataSource.setUrl(env.getProperty("accounts.datasource.url"));
      |     return dataSource;
      |     }
      |}
      |""".stripMargin

  "ConfigFilePass" should {
    "create a file node for the property file" in {
      val List(name: String) = cpg.file.name.l
      name.endsWith("/test.properties") shouldBe true
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("accounts.datasource.url")
        .contains("jdbc:mariadb://localhost:3306/accounts?useSSL=false") shouldBe true
      properties.get("internal.logger.api.base").contains("https://logger.privado.ai/")
    }

    "connect property nodes to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename.endsWith("/test.properties") shouldBe true
    }

    "connect property node to literal via `IS_USED_AT` edge" in {
      val List(lit: Literal) = cpg.property.usedAt.l
      lit.code shouldBe "\"accounts.datasource.url\""
    }
    "connect literal node to property via `ORIGINAL_PROPERTY` edge" in {
      val List(javaP: JavaProperty) = cpg.property.usedAt.originalProperty.l
      javaP.value shouldBe "jdbc:mariadb://localhost:3306/accounts?useSSL=false"
    }
  }
}

/** Base class for tests on properties files and Java code.
  */
abstract class PropertiesFilePassTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val configFileContents: String
  val javaFileContents: String

  var cpg: Cpg       = _
  var inputDir: File = _

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "test.properties").write(configFileContents)
    (inputDir / "GeneralConfig.java").write(javaFileContents)
    (inputDir / "unrelated.file").write("foo")
    val config = Config(inputPath = inputDir.toString())
    cpg = new JavaSrc2Cpg().createCpg(config).get
    new PropertiesFilePass(cpg, inputDir.toString).createAndApply()
    super.beforeAll()
  }
}
