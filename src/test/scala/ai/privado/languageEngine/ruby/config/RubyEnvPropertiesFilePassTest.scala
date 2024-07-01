package ai.privado.languageEngine.ruby.config

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.languageEngine.java.language.*
import ai.privado.languageEngine.ruby.passes.config.RubyEnvPropertyLinkerPass
import ai.privado.model.{Constants, Language}
import ai.privado.model.Language
import ai.privado.testfixtures.RubyFrontendTestSuite
import ai.privado.utility.PropertyParserPass
import better.files.File
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RubyEnvPropertiesFilePassTest extends RubyFrontendTestSuite {

  val mongourl = "mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority"

  "EnvConfigFilePass" should {

    val cpg = code(
      """
        |MONGO_URL=mongodb+srv://myuser:mypassword@mycluster.abc123.mongodb.net/mydatabase?retryWrites=true&w=majority
        |DB_NAME=mydb
        |""".stripMargin,
      "config.env"
    )
      .moreCode(
        """
      |mongo_url = ENV["MONGO_URL"]
      |db_name = ENV.fetch["DB_NAME"]
      |""".stripMargin,
        "code.rb"
      )

    "create a file node for config file" in {
      val files = cpg.file.name.l
      files.filter(_.endsWith(".env")).head.endsWith("config.env") shouldBe true
    }

    "create a 'property' node" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties.get("MONGO_URL").contains(mongourl) shouldBe true
    }

    "connect property node to file" in {
      val List(filename: String) = cpg.property.file.name.dedup.l
      filename.endsWith("config.env") shouldBe true
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

//TODO Having issue in detection of API call, need to fix that before
class RubyEnvPropertyLinkerPassTest extends RubyPropertiesFilePassTestBase(".yaml") {

  override val configFileContents: String =
    """
      |config:
      |  default:
      |    API_URL: http://exampleKubernetesService
      |""".stripMargin

  override val codeFileContents: String =
    """
      |require 'net/http'
      |
      |api_url = ENV["API"]
      |url = URI.parse(api_url)
      |response = Net::HTTP.get_response(url)
      |""".stripMargin

  "Http client execute API Sample" ignore {
    "Http Client execute should be tagged" in {
      val callNode = cpg.call.name("get_response").head
      callNode.tag.size shouldBe 6
      callNode.tag
        .nameExact(Constants.id)
        .head
        .value shouldBe (Constants.thirdPartiesAPIRuleId) + ".exampleKubernetesService"
      callNode.tag.nameExact(Constants.catLevelOne).head.value shouldBe Constants.sinks
      callNode.tag.nameExact(Constants.catLevelTwo).head.value shouldBe Constants.third_parties
      callNode.tag.nameExact(Constants.nodeType).head.value shouldBe "api"
      callNode.tag
        .nameExact("third_partiesapi")
        .head
        .value shouldBe (Constants.thirdPartiesAPIRuleId + ".exampleKubernetesService")
      callNode.tag.nameExact("apiUrlSinks.ThirdParties.API.exampleKubernetesService")
    }

    "create a `property` node for each property" in {
      val properties = cpg.property.map(x => (x.name, x.value)).toMap
      properties
        .get("config.default.API_URL")
        .contains("http://exampleKubernetesService")
    }

    "Two way edge between member and propertyNode" in {
      val properties = cpg.property.usedAt.originalProperty.l.map(property => (property.name, property.value)).toMap
      properties
        .get("config.default.API_URL")
        .contains("http://exampleKubernetesService") shouldBe true
    }
  }

}
