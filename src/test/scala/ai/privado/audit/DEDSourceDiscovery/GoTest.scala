package ai.privado.audit

import ai.privado.audit.DEDSourceDiscovery
import ai.privado.languageEngine.go.GoTestBase
import ai.privado.model.{Language}

import scala.util.Try
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class GoDEDSourceDiscoveryTest extends GoTestBase {
  "Check ded source discovery results" in {
    val (cpg, _) = code("""
        package main
        |
        |import "fmt"
        |
        |type User struct {
        |	FirstName string
        |	Age       int
        |	Location  string
        |	Email     string // 10
        |	passwd    string // 11 Note: this field is unexported
        |}
        |
        |func main() {
        |	// Creating a user instance
        |	user := User{
        |		FirstName: "John Doe",
        |		Age:       25,
        |		Location:  "New York",
        |		Email:     "abc@gmail.com", //20
        |		passwd:    "yourPassword", //21
        |	}
        |}
        |""".stripMargin)
    val dedSources = DEDSourceDiscovery.processDEDSourceDiscovery(Try(cpg), Language.GO).drop(1)
    val expectedResult = List(
      "Location,Member,main.User",
      "FirstName,Member,main.User",
      "Email,Member,main.User",
      "Age,Member,main.User",
      "passwd,Member,main.User",
      "user,Identifier,main.User"
    )
    val res: List[String] = dedSources.map(i => {
      s"${i(3)},${i(12)},${i(0)}"
    })
    res shouldBe expectedResult
  }

}
