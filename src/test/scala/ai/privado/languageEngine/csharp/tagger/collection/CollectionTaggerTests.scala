package ai.privado.languageEngine.csharp.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{
  CatLevelOne,
  ConfigAndRules,
  Constants,
  FilterProperty,
  Language,
  NodeType,
  RuleInfo,
  SourceCodeModel
}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.CSharpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class CollectionTaggerTests extends CSharpFrontendTestSuite {

  var sinkRules: List[RuleInfo] = List(
    RuleInfo(
      "Loggers.Console",
      "WriteLine",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*WriteLine.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.CSHARP,
      Array()
    )
  )

  var collectionRules = List(
    RuleInfo(
      "Collections.Mvc",
      "ASPNet MVC Endpoints",
      "",
      FilterProperty.CODE,
      Array(),
      List("(?i).*(Route|HttpGet|HttpPost|HttpPut).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = Constants.annotations,
      Language.CSHARP,
      Array()
    )
  )

  val ruleCache = new RuleCache().setRule(
    RuleInfoTestData.rule.copy(sources = RuleInfoTestData.sourceRule, sinks = sinkRules, collections = collectionRules)
  )

  "Types with Route annotations having [controller]" should {
    val cpg = code(
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
    ).withRuleCache(ruleCache)

    "be tagged as part of Collection tagger" in {

      val List(emailMethod) = cpg.method.nameExact("GetEmail").l
      emailMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      emailMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/some/{email}")

      val List(listMethod) = cpg.method.nameExact("Print").l
      listMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      listMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/some/print/{firstName}")
    }
  }

  "Methods having [controller] annotations" should {
    val cpg = code(
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
    ).withRuleCache(ruleCache)

    "be tagged as part of Collection tagger" in {

      val List(copyMethod) = cpg.method.nameExact("Copy").l
      copyMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      copyMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/some/copy")
    }
  }

  "Methods having implicit routes" should {
    val cpg = code(
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
    ).withRuleCache(ruleCache)

    "be tagged as part of Collection tagger" in {

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
    val cpg = code(
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
    ).withRuleCache(ruleCache)

    "have only relevant annotations tagged as collections" in {

      val List(createMethod) = cpg.method.nameExact("Job").l
      createMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      createMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/v{version:apiVersion}/my/job")
    }
  }

  "Route annotations with more than one arguments" should {
    val cpg = code(
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
    ).withRuleCache(ruleCache)

    "be tagged as collections correctly" in {

      val List(fetchMethod) = cpg.method.nameExact("Fetch").l
      fetchMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      fetchMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/v{version:apiVersion}/my/{id}")

      val List(createMethod) = cpg.method.nameExact("Create").l
      createMethod.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
      createMethod.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List("/api/v{version:apiVersion}/my/create")
    }
  }
}
