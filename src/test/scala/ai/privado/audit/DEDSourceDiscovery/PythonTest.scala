package ai.privado.audit

import ai.privado.audit.DEDSourceDiscovery
import ai.privado.languageEngine.python.PrivadoPySrc2CpgFixture
import ai.privado.model.{Language}

import scala.util.Try
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class PythonDEDSourceDiscoveryTest extends PrivadoPySrc2CpgFixture {
  "Check ded source discovery results" in {
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
    val dedSources = DEDSourceDiscovery.processDEDSourceDiscovery(Try(cpg.asInstanceOf[Cpg]), Language.PYTHON).drop(1)
    val expectedResult = List(
      "complete_login,Member,Test0.py:<module>.EventbriteOAuth2Adapter<meta>",
      "EventbriteOAuth2Adapter,Member,Test0.py:<module>",
      "kwargs,Method Parameter,ANY",
      "accountId,Method Parameter,ANY",
      "extra_data,Identifier,requests.py:<module>.get.<returnValue>.json.<returnValue>",
      "client,FieldIdentifier,NA",
      "token,Method Parameter,ANY",
      "sociallogin_from_response,FieldIdentifier,NA"
    )
    val res: List[String] = dedSources.map(i => {
      s"${i(3)},${i(12)},${i(0)}"
    })
    res shouldBe expectedResult
  }

}
