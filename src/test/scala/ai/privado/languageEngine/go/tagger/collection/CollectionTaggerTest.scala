package ai.privado.languageEngine.go.tagger.collection

import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

class CollectionFuncTaggerTest extends CollectionTaggerTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContentMap: Map[String, String] = getContent()

  private def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put(
      "routes",
      """
        |package main
        |
        |import (
        | "fmt"
        | "github.com/gorilla/mux"
        |)
        |
        |func main() {
        | myRouter := mux.NewRouter().StrictSlash(true)
        |
        | myRouter.HandleFunc("/articles", allArticles).Methods("GET")
        |}
        |
        |func allArticles() {
        | firstName := "anything"
        |	fmt.Println(firstName)
        |}
        |""".stripMargin
    )

    testClassMap.toMap
  }

  "CollectionTagger" should {
    "Test correct collection tagging" in {
      cpg.method("allArticles").tag.nameExact("COLLECTION_METHOD_ENDPOINT").head.value shouldBe "\"/articles\""
    }
  }
}

class CollectionTaggerGinFrameworkTest extends CollectionTaggerTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContentMap: Map[String, String] = getContent()

  private def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put(
      "routes",
      """
        |package main
        |
        |import (
        | "fmt"
        | "github.com/gin-gonic/gin"
        |)
        |
        |func fetchArticle(r *gin.RouterGroup) {
        | r.POST("/articles", allArticles)
        |}
        |
        |func allArticles() {
        | firstName := "anything"
        |	fmt.Println(firstName)
        |}
        |""".stripMargin
    )

    testClassMap.toMap
  }

  "CollectionTagger" should {
    "Test correct collection tagging" in {
      cpg.method("allArticles").tag.nameExact("COLLECTION_METHOD_ENDPOINT").head.value shouldBe "\"/articles\""
    }
  }
}

class CollectionTaggerEchoFrameworkTest extends CollectionTaggerTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContentMap: Map[String, String] = getContent()

  private def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put(
      "routes",
      """
        |package main
        |
        |import (
        | "fmt"
        | "github.com/labstack/echo/v4"
        | "github.com/labstack/echo/v4/middleware"
        |)
        |
        |func fetchArticle() {
        | e := echo.New()
        | e.Use(middleware.Logger())
        |
        | e.GET("/articles", allArticles)
        |}
        |
        |func allArticles() {
        | firstName := "anything"
        |	fmt.Println(firstName)
        |}
        |""".stripMargin
    )

    testClassMap.toMap
  }

  "CollectionTagger" should {
    "Test correct collection tagging" in {
      cpg.method("allArticles").tag.nameExact("COLLECTION_METHOD_ENDPOINT").head.value shouldBe "\"/articles\""
    }
  }
}

class CollectionTaggerChiFrameworkTest extends CollectionTaggerTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContentMap: Map[String, String] = getContent()

  private def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put(
      "routes",
      """
        |package main
        |
        |import (
        | "fmt"
        | "github.com/go-chi/chi"
        |)
        |
        |func fetchArticle() {
        | router := app.router
        | router.Group(func(r chi.Router) {
        | r.Use(app.logger.NewRequestLogger())
        |		router.Get("/articles", allArticles)
        | })
        |}
        |
        |func allArticles() {
        | firstName := "anything"
        |	fmt.Println(firstName)
        |}
        |""".stripMargin
    )

    testClassMap.toMap
  }

  // ast.FuncLit AST type is not handled yet.
  "CollectionTagger" ignore {
    "Test correct collection tagging" in {
      cpg.method("allArticles").tag.nameExact("COLLECTION_METHOD_ENDPOINT").head.value shouldBe "\"/articles\""
    }
  }
}

class CollectionTaggerGoFiberFrameworkTest extends CollectionTaggerTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContentMap: Map[String, String] = getContent()

  private def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put(
      "routes",
      """
        |package main
        |
        |import (
        | "fmt"
        | "github.com/NikSchaefer/go-fiber/middleware"
        | "github.com/gofiber/fiber/v2"
        |)
        |
        |func fetchArticle(router *fiber.App) {
        | router.Use(middleware.Security)
        | router.Post("/articles", allArticles)
        |}
        |
        |func allArticles() {
        | firstName := "anything"
        |	fmt.Println(firstName)
        |}
        |""".stripMargin
    )

    testClassMap.toMap
  }

  "CollectionTagger" should {
    "Test correct collection tagging" in {
      cpg.method("allArticles").tag.nameExact("COLLECTION_METHOD_ENDPOINT").head.value shouldBe "\"/articles\""
    }
  }
}

class CollectionTaggerGoBuffaloFrameworkTest extends CollectionTaggerTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContentMap: Map[String, String] = getContent()

  private def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put(
      "routes",
      """
        |package main
        |
        |import (
        | "fmt"
        | "github.com/gobuffalo/buffalo"
        |)
        |
        |func fetchArticle(router *fiber.App) {
        |  app = buffalo.New(buffalo.Options{
        |			Env:         "env",
        |			SessionName: "_toodo_session",
        |	  })
        |
        |  app.GET("/articles", handler.allArticles)
        |}
        |""".stripMargin
    )

    testClassMap.put(
      "handler",
      """
        |package handler
        |
        |func allArticles() {
        | firstName := "anything"
        |	fmt.Println(firstName)
        |}
        |""".stripMargin
    )

    testClassMap.toMap
  }

  "CollectionTagger" should {
    "Test correct collection tagging" in {
      cpg.method("allArticles").tag.nameExact("COLLECTION_METHOD_ENDPOINT").head.value shouldBe "\"/articles\""
    }
  }
}
