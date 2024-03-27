package ai.privado.languageEngine.python

import ai.privado.cache.RuleCache
import ai.privado.passes.*
import ai.privado.utility.PropertyParserPass
import io.joern.pysrc2cpg.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class PythonDataFlowTests extends PrivadoPySrc2CpgFixture {

  "Flows within an OAuth adapter" should {
    val cpg = code("""import boto
        |import requests
        |
        |class EventbriteOAuth2Adapter(OAuth2Adapter):
        |
        |    def getFirstName(self):
        |        return "https://www.google.com/v3/users/me/"
        |
        |    def complete_login(self, request, accountId, token, **kwargs):
        |        resp = requests.get(self.getFirstName(), params=accountId)
        |        extra_data = resp.json()
        |
        |        boto.client("First Name: " + self.getFirstName)
        |        return self.get_provider().sociallogin_from_response(request,
        |                                                             extra_data)
        |""".stripMargin)

    "find a flow between a parameter identifier and a call" in {
      val source = cpg.identifier("accountId").lineNumber(9)
      val sink   = cpg.call("get").lineNumber(10)
      sink.reachableByFlows(source).size shouldBe 1
    }

    "find a flow between a literal return and a call" in {
      val source = cpg.literal.code(".*google.*").lineNumber(7)
      val sink   = cpg.call("get").lineNumber(10)
      sink.reachableByFlows(source).size shouldBe 1
    }
  }

}
