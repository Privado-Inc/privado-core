package ai.privado.languageEngine.c.passes

import ai.privado.entrypoint.PrivadoInput
import ai.privado.testfixtures.CFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class CTypeRecoveryTests extends CFrontendTestSuite {

  "Type Recovery (case 1)" should {
    val cpg = code(
      """
        |class LinkingManager
        |{
        |   bool LoggedIn()
        |   {
        |       return true;
        |   }
        |
        |   bool LoggedOut()
        |   {
        |       return true;
        |   }
        |};
        |extern LinkingManager* GetLinkingManager();
        |""".stripMargin,
      "LinkingManager.h"
    ).moreCode("""
        |#include "LinkingManager.h"
        |void MyFunc()
        |{
        |     bool returnValue = GetLinkingManager()->LoggedIn();
        |
        |     GetLinkingManager()->LoggedOut();
        |};
        |""".stripMargin)
      .withPrivadoInput(privadoInput = PrivadoInput(isSkipHeaderFileContext = true))
    "resolve types via method name placed with assignment" in {
      val List(loggedIn) = cpg.call("LoggedIn").l
      loggedIn.methodFullName shouldBe "LinkingManager.LoggedIn"
    }

    "resolve types via method name placed without assignment" in {
      val List(loggedIn) = cpg.call("LoggedOut").l
      loggedIn.methodFullName shouldBe "LinkingManager.LoggedOut"
    }
  }

}
