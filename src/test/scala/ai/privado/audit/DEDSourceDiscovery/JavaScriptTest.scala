package ai.privado.audit

import ai.privado.audit.DEDSourceDiscovery
import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import ai.privado.rule.RuleInfoTestData
import ai.privado.model.Language
import scala.util.Try
import io.shiftleft.codepropertygraph.generated.Cpg

import scala.collection.mutable.ListBuffer
import io.shiftleft.semanticcpg.language.*

class JSDEDSourceDiscoveryTest extends JavaScriptFrontendTestSuite {
  "Check ded source discovery results" in {
    val cpg = Try(code("""
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
                     |""".stripMargin).asInstanceOf[Cpg])
    val dedSources = DEDSourceDiscovery.processDEDSourceDiscovery(cpg, Language.JAVASCRIPT).drop(1)
    val expectedResult = List(
      "emailId,Method Parameter,ANY",
      "providedIn,FieldIdentifier,NA",
      "Auth,Identifier,ANY",
      "accountId,Identifier,__ecma.String",
      "passwd,Method Parameter,ANY",
      "SharedService,Identifier,ANY",
      "userName,Method Parameter,__ecma.String",
      "saveUserFeedback,Member,Test0.js::program:SharedService",
      "payload,Method Parameter,ANY"
    )
    val res: List[String] = dedSources.map(i => {
      s"${i(3)},${i(12)},${i(0)}"
    })
    res shouldBe expectedResult
  }
}
