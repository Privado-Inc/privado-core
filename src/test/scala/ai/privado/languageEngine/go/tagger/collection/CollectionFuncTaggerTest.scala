package ai.privado.languageEngine.go.tagger.collection

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

abstract class CollectionTaggerTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg = _
  val goFileContentMap: Map[String, String]
  var inputDir: File  = _
  var outputDir: File = _
  val ruleCache       = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    for ((key, content) <- goFileContentMap) {
      println("key ---- " + key)
      println("content ----- " + content)
      (inputDir / s"$key.go").write(content)
      println("ppppppp")
    }
    outputDir = File.newTemporaryDirectory()

    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputDir.pathAsString)
    val goSrc  = new GoSrc2Cpg()
    val xtocpg = goSrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

    cpg = xtocpg.get

    ruleCache.setRule(rule)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputDir.delete()
    super.afterAll()
  }

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      Array(),
      List("(?i).*firstName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.GO,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, List(), List(), List(), List(), List(), List(), List(), List(), List())
  val taggerCache = new TaggerCache()
}

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
