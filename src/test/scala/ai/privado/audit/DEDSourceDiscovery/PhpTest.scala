package ai.privado.audit

import ai.privado.audit.DEDSourceDiscovery
import ai.privado.languageEngine.php.PhpTestBase
import ai.privado.model.Language

import scala.util.Try
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class PhpDEDSourceDiscoveryTest extends PhpTestBase {
  "Check ded source discovery results" in {
    val (cpg, _) = code(
      """
        |<?php
        |
        |  class User {
        |    public $firstName;
        |    public $lastName;
        |    public $age;
        |    public $email;
        |    public $dob;
        |
        |    function __construct($fname, $lname, $userAge, $userEmail, $userDob) {
        |      $this->firstName = $fname;
        |      $this->lastName = $lname;
        |      $this->age = $userAge;
        |      $this->email = $userEmail;
        |      $this->dob = $userDob;
        |    }
        |  }
        |
        |  $user = new User("a", "b", 1, "c@d.com", "01-01-90");
        |  echo $user->firstName;
        |?>
        |
        |""".stripMargin)

    val dedSources = DEDSourceDiscovery.processDEDSourceDiscovery(Try(cpg), Language.PHP).drop(1)
    val expectedResult = List(
      "email,Member,User",
      "dob,Member,User",
      "userEmail,Method Parameter,ANY",
      "fname,Method Parameter,ANY",
      "age,Member,User",
      "lastName,Member,User",
      "firstName,Member,User",
      "userDob,Method Parameter,ANY",
      "userAge,Method Parameter,ANY",
      "user,Identifier,User",
      "lname,Method Parameter,ANY"
    )
    val res: List[String] = dedSources.map(i => {
      s"${i(3)},${i(12)},${i(0)}"
    })
    res shouldBe expectedResult
  }

}
