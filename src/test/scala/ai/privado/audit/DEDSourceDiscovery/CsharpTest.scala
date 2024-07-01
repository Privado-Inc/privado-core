package ai.privado.audit

import ai.privado.audit.DEDSourceDiscovery
import ai.privado.testfixtures.CSharpFrontendTestSuite
import ai.privado.model.Language

import scala.util.Try
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class ChsarpDEDSourceDiscoveryTest extends CSharpFrontendTestSuite  {
  "Check ded source discovery results" in {
    val sourceCode =
      """
        |using System;
        |
        |// Define the User class with required properties
        |public class User
        |{
        |    public string firstName { get; }
        |    public string passwd { get; }
        |    public string emailId { get; }
        |
        |    public User(string firstName, string passwd, string emailId)
        |    {
        |        firstName = firstName;
        |        passwd = passwd;
        |        emailId = emailId;
        |    }
        |
        |    public override string ToString()
        |    {
        |        return $"User(FirstName={firstName}, Passwd={passwd}, EmailId={emailId})";
        |    }
        |}
        |
        |class Program
        |{
        |    static void Main()
        |    {
        |        // Create an instance of the User class
        |        User user = new User(
        |            "firstName1",
        |            "yourPassword",
        |            "yourEmail@example.com"
        |        );
        |
        |        // Access and print the properties
        |        Console.WriteLine(user);
        |    }
        |}
        |""".stripMargin
    val cpg = code(sourceCode, "Test0.cs")
    val dedSources = DEDSourceDiscovery.processDEDSourceDiscovery(Try(cpg), Language.CSHARP).drop(1)
    val expectedResult = List(
      "emailId,Member,User",
      "firstName,Member,User",
      "passwd,Member,User",
      "user,Identifier,User"
    )
    val res: List[String] = dedSources.map(i => {
      s"${i(3)},${i(12)},${i(0)}"
    })
    res shouldBe expectedResult
  }

}
