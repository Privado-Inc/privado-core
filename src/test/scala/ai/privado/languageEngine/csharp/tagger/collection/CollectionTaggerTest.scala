package ai.privado.languageEngine.csharp.tagger.collection

import ai.privado.languageEngine.csharp.CSharpTestBase
import ai.privado.model.{CatLevelOne, Constants, SourceCodeModel}
import io.shiftleft.semanticcpg.language.*

class CollectionTaggerTest extends CSharpTestBase {

  "Types with Route annotations in code" should {
    "be tagged as part of Collection tagger" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
            """
              |using System;
              |
              |namespace Foo {
              |
              |  [Route("api/[controller]")]
              |  [ApiController]
              |  public class SomeController : ControllerBase
              |  {
              |    [HttpGet("{email}")]
              |    public IActionResult GetEmail(string email)
              |    {
              |       return ControllerContext.MyDisplayRouteInfo(email);
              |    }
              |
              |    [HttpGet("[action]/{firstName}")]
              |    public IActionResult Print(string firstName)
              |    {
              |       return ControllerContext.MyDisplayRouteInfo(firstName);
              |    }
              |  }
              |}
              |""".stripMargin,
            "Test.cs"
          )
        )
      )

      val List(emailMethod) = cpg.method.nameExact("GetEmail").l
      emailMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      emailMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/some/{email}")

      val List(listMethod) = cpg.method.nameExact("Print").l
      listMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      listMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/some/print/{firstName}")
    }
  }

}
