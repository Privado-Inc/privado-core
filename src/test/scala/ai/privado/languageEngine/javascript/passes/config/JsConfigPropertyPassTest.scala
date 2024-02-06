package ai.privado.languageEngine.javascript.passes.config

import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.languageEngine.java.language.NodeStarters
import io.shiftleft.semanticcpg.language._

class JsConfigPropertyPassTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "Javascript config property pass" should {
    "able to create property node" in {

      val cpg = code("""
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
          |""".stripMargin)

      cpg.property.size shouldBe 14
      cpg.property.nameExact("module.exports.database.host").value.l shouldBe List("mongodb://5.3.34.3:27017")
      cpg.property.nameExact("window.CONFIG.auth0.cookieConfig.domain").value.l shouldBe List(".mydomain.com")

    }
  }

}

def code(code: String) = {
  val inputDir   = File.newTemporaryDirectory()
  val outputFile = File.newTemporaryFile()

  (inputDir / "config.js").write(code)

  val cpgconfig = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
  val cpg       = new JsSrc2Cpg().createCpgWithAllOverlays(cpgconfig).get

  X2Cpg.applyDefaultOverlays(cpg)

  new JsConfigPropertyPass(cpg).createAndApply()
  cpg
}
