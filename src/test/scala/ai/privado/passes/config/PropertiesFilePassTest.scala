/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

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

      val List(param: MethodParameterIn) = cpg.property.usedAt.l
      param.originalProperty.head.value shouldBe "https://logger.privado.ai/"
      param.originalPropertyValue.head shouldBe "https://logger.privado.ai/"
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

      val List(lit: Literal) = cpg.property.usedAt.l
      lit.originalProperty.head.value shouldBe "jdbc:mariadb://localhost:3306/accounts?useSSL=false"
      lit.originalPropertyValue.head shouldBe "jdbc:mariadb://localhost:3306/accounts?useSSL=false"
    }
  }
}

/** Base class for tests on properties files and Java code.
  */
abstract class PropertiesFilePassTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val configFileContents: String
  val javaFileContents: String
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

  override def afterAll(): Unit = {
    inputDir.delete()
    super.afterAll()
  }

}
