package ai.privado.languageEngine.python.tagger.collection

import ai.privado.languageEngine.python.PrivadoPySrc2CpgFixture
import ai.privado.rule.RuleInfoTestData
import io.shiftleft.semanticcpg.language.*
import ai.privado.tagger.sink.api.CollectionValidator

class CherryPyTaggerTest extends PrivadoPySrc2CpgFixture with CollectionValidator {
  "cherrypy collection tagger" should {
    val cpg = code("""
        |from cherrypy import dispatch
        |
        |mapping = dispatch.RoutesDispatcher()
        |
        |mapping.connect('send_to_user', 'api/v%s/user/:name/' % version, controller=server, action='send_to_user', conditions=dict(method=['POST']))
        |""".stripMargin).moreCode("""
        |def send_to_user(firstName):
        |   print(firstName)
        |""".stripMargin)

    new CherryPyTagger(cpg, RuleInfoTestData.ruleCache).createAndApply()

    "tag collection methods" in {
      val List(sendToUserMethod) = cpg.method("send_to_user").l
      assertCollectionMethod(sendToUserMethod)
      assertCollectionUrl(sendToUserMethod, "api/v%s/user/:name/")
      assertCollectionInFinalJson(cpg, 1)
    }
  }
}
