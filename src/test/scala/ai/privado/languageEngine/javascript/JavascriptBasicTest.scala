package ai.privado.languageEngine.javascript

import ai.privado.model.Constants
import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.semanticcpg.language.*

class JavascriptBasicTest extends JavaScriptFrontendTestSuite {

  "Dummy test to write test case" should {
    val cpg = code("""
                     |import { HttpClient } from '@angular/common/http';
                     |
                     |@Injectable({
                     |  providedIn: 'root',
                     |})
                     |export class SharedService {
                     |  constructor(private http: HttpClient) {}
                     |
                     |  saveUserFeedback(payload) {
                     |    return this.http.post(
                     |      'albert-the-cat@jobcloud.ch',
                     |      payload
                     |    );
                     |  }
                     |}
                     |
                     |
                     |export class Auth {
                     |  login(emailId, passwd, userName = "guest-user") {
                     |    const accountId = "jhgejkrbg";
                     |    console.log(emailId, passwd, userName);
                     |  }
                     |}
                     |""".stripMargin)

    "show using only cpg" in {
      cpg.identifier("emailId").nonEmpty shouldBe true
    }

    "show using output json" in {
      val outputJson = cpg.getPrivadoJson()
      outputJson(Constants.repoName).asString.get should startWith("x2cpgTestTmpDir")
    }
  }
}
