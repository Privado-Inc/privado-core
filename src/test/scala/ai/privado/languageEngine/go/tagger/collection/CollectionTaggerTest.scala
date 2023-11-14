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

class CollectionRestTaggerTest extends CollectionTaggerTestBase {

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
