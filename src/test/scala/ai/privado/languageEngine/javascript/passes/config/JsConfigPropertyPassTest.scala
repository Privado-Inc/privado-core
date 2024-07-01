package ai.privado.languageEngine.javascript.passes.config

import ai.privado.entrypoint.PrivadoInput
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class JsConfigPropertyPassTest extends JavaScriptFrontendTestSuite {

  val privadoInput = PrivadoInput(assetDiscovery = true)

  "Javascript config property pass" should {

    val cpg = code(
      """
          |
          |window.CONFIG = {
          |    auth0: {
          |      authEnvironment: 'integration',
          |      clientId: 'e8irdMtEkl7NgzCQEVTajdaetghn8Ha3',
          |      cookieConfig: {
          |        domain: '.mydomain.com',
          |        expires: 1,
          |        name: 'jwt',
          |        path: '/',
          |        secure: false,
          |      },
          |      databaseConnection: 'User-Password',
          |      domain: 'auth.integration.mydomain.com',
          |      enableCookieBackwardsCompatability: true
          |   }
          |};
          |
          |module.exports = {
          |    database: {
          |      host: 'mongodb://5.3.34.3:27017',
          |      name: 'mydatabase',
          |      user: 'mydatabaseuser',
          |      password: 'mydatabasepassword',
          |    },
          |    // Other configuration options...
          |  };
          |
          |""".stripMargin,
      "config.js"
    ).withPrivadoInput(privadoInput)

    "able to create property node" in {
      cpg.property.size shouldBe 14
      cpg.property.nameExact("module.exports.database.host").value.l shouldBe List("mongodb://5.3.34.3:27017")
      cpg.property.nameExact("window.CONFIG.auth0.cookieConfig.domain").value.l shouldBe List(".mydomain.com")
    }
  }
}

class JsonPropertyTests extends JavaScriptFrontendTestSuite {

  "JSON file having array nodes" should {

    val cpg = code(
      """
        |{
        |    "databases": [
        |      {
        |        "name": "MySQL Database",
        |        "uri": "mysql://username:password@hostname:port/database_name"
        |      }
        |     ],
        |     "mongoUri" : "mongodb://username:password@hostname:port/database_name"
        |}
        |""".stripMargin,
      "test.json"
    )

    "get parsed and property nodes should be generated" in {
      cpg.property.map(p => (p.name, p.value)).l shouldBe List(
        ("databases[0].name", "MySQL Database"),
        ("databases[0].uri", "mysql://username:password@hostname:port/database_name"),
        ("mongoUri", "mongodb://username:password@hostname:port/database_name")
      )
    }
  }
}
