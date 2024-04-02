package ai.privado.languageEngine.csharp.tagger.collection

import ai.privado.languageEngine.csharp.CSharpTestBase
import ai.privado.model.{CatLevelOne, Constants, SourceCodeModel}
import io.shiftleft.semanticcpg.language.*

class CollectionTaggerTest extends CSharpTestBase {

  "Types with Route annotations having [controller]" should {
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
              |
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

  "Methods having [controller] annotations" should {
    "be tagged as part of Collection tagger" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
            """
              |using System;
              |
              |namespace Foo {
              |
              |  public class SomeController : ControllerBase
              |  {
              |    [HttpGet("api/[controller]/[action]")]
              |    public IActionResult Copy(string email)
              |    {
              |       return ControllerContext.MyDisplayRouteInfo(email);
              |    }
              |  }
              |}
              |""".stripMargin,
            "Test.cs"
          )
        )
      )

      val List(copyMethod) = cpg.method.nameExact("Copy").l
      copyMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      copyMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/some/copy")
    }
  }

  "Methods having implicit routes" should {
    "be tagged as part of Collection tagger" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
            """
              |using System;
              |
              |namespace Foo {
              |
              |  [Route("api/[controller]/[action]")]
              |  public class SomeController : ControllerBase
              |  {
              |    [HttpGet]
              |    public IActionResult Copy(string email)
              |    {
              |       return ControllerContext.MyDisplayRouteInfo(email);
              |    }
              |
              |    [HttpGet("{id}")]
              |    public IActionResult Paste(string email)
              |    {
              |       return ControllerContext.MyDisplayRouteInfo(email);
              |    }
              |
              |  }
              |
              |  [Route("api/[controller]")]
              |  public class OtherController : ControllerBase
              |  {
              |    [HttpPost]
              |    public IActionResult Cut(string email)
              |    {
              |       return ControllerContext.MyDisplayRouteInfo(email);
              |    }
              |  }
              |
              |}
              |""".stripMargin,
            "Test.cs"
          )
        )
      )

      val List(copyMethod) = cpg.method.nameExact("Copy").l
      copyMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      copyMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/some/copy")

      val List(pasteMethod) = cpg.method.nameExact("Paste").l
      pasteMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      pasteMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/some/paste/{id}")

      val List(cutMethod) = cpg.method.nameExact("Cut").l
      cutMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      cutMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/other/cut")
    }
  }

  "Different order of annotations and/or extra annotations" should {
    "have only relevant annotations tagged as collections" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
            """
              |namespace Foo.Bar;
              |
              |[ApiController]
              |[ApiVersion("1")]
              |[Route("api/v{version:apiVersion}/[controller]")]
              |public class MyController : ControllerBase
              |{
              |
              |    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(MyResponse))]
              |    [HttpPost]
              |    public async Task<IActionResult> Job(MyRequest email)
              |    {
              |        return Ok(response);
              |    }
              |}
              |""".stripMargin,
            "Test.cs"
          )
        )
      )

      val List(createMethod) = cpg.method.nameExact("Job").l
      createMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      createMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/v{version:apiVersion}/my/job")
    }
  }

  "Route annotations with more than one arguments" should {
    "be tagged as collections correctly" in {
      val (cpg, _) = code(
        List(
          SourceCodeModel(
            """
              |namespace Foo.Bar;
              |[Route("api/v{version:apiVersion}/[controller]")]
              |[ApiVersion("1.0")]
              |[ApiController]
              |public class MyController : ControllerBase
              |{
              |    [HttpGet("{id}", Name = nameof(MyStuff))]
              |    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(MyResponse))]
              |    public async Task<IActionResult> Fetch(Email email)
              |    {
              |        return Ok(response);
              |    }
              |
              |    [HttpPost(Name = nameof(MyStuff))]
              |    [ProducesResponseType(StatusCodes.Status201Created, Type = typeof(MyResponse))]
              |    [ProducesResponseType(typeof(MyResponse), 400)]
              |    public async Task<IActionResult> Create(MyRequest email)
              |    {
              |        return Ok(response);
              |    }
              |}
              |""".stripMargin,
            "Test.cs"
          )
        )
      )

      val List(fetchMethod) = cpg.method.nameExact("Fetch").l
      fetchMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      fetchMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/v{version:apiVersion}/my/{id}")

      val List(createMethod) = cpg.method.nameExact("Create").l
      createMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      createMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/v{version:apiVersion}/my/create")
    }
  }

}
