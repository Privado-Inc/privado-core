package ai.privado.audit

import ai.privado.audit.DEDSourceDiscovery
import ai.privado.model.Language
import ai.privado.testfixtures.JavaFrontendTestSuite
import scala.util.Try
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class JavaDEDSourceDiscoveryTest extends JavaFrontendTestSuite {
  "Check ded source discovery results" in {
    val cpg = Try(code(
      """
        |public class User {
        |   public String userName;
        |   public String passwd;
        |   public String emailId;
        |
        |   public String getUserName() {return userName;}
        |   public void setUserName(String firstName) {this.userName = userName;}
        |}
        |""".stripMargin,
      "User.java"
    ).asInstanceOf[Cpg])

    val dedSources = DEDSourceDiscovery.processDEDSourceDiscovery(cpg, Language.JAVA).drop(1)
    val expectedResult = List(
      "emailId,Member,User",
      "passwd,Member,User",
      "userName,Member,User"
    )
    val res: List[String] = dedSources.map(i => {
      s"${i(3)},${i(12)},${i(0)}"
    })
    res shouldBe expectedResult

  }

}
