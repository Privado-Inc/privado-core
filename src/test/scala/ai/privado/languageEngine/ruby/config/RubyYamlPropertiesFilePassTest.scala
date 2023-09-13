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

class RubyYamlPropertiesFilePassTest extends RubyPropertiesFilePassTestBase(".yml") {
  val base_url = "https://example.com/api/v1"
  override val configFileContents: String =
    """
      |foo:
      |  api_base_url: "https://example.com/api/v1"
      |""".stripMargin

  override val codeFileContents: String =
    """
      |COMPANIES_URL = "#{Settings.foo.api_base_url}/bar".freeze
      |""".stripMargin

  "YamlConfigFilePass" should {
    "create a file node for config file" in {
      val files = cpg.file.name.l
      files.filter(_.endsWith(".yml")).head.endsWith("config.yml") shouldBe true
    }

    "create a 'property' node" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties.get("foo.api_base_url").contains(base_url) shouldBe true
    }

    "connect property node to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename.endsWith("config.yml") shouldBe true
    }

    "connect property node to literal via `IS_USED_AT` edge" in {
      val lit = cpg.property.usedAt.l.head
      lit.code shouldBe "\"#{Settings.foo.api_base_url}/bar\".freeze"
    }

    "connect literal node to property via `ORIGINAL_PROPERTY` edge" in {
      val javaP = cpg.property.usedAt.originalProperty.l.head
      javaP.value shouldBe base_url

      val lit = cpg.property.usedAt.l.head
      lit.originalProperty.head.value shouldBe base_url
      lit.originalPropertyValue.head shouldBe base_url
    }
  }
}