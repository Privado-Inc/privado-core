package ai.privado.languageEngine.go.tagger.sink

import ai.privado.cache.AppCache
import ai.privado.exporter.ProbableSinkExporter
import ai.privado.model.Language
import ai.privado.tagger.sink.RegularSinkTagger

import scala.collection.mutable

class ProbableSinkTest extends ProbableSinkTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new RegularSinkTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContentMap: Map[String, String] = getContent()

  private def getContent(): Map[String, String] = {
    val testClassMap = mutable.Map[String, String]()

    testClassMap.put(
      "main",
      """
        |package main
        |
        |import (
        | "log"
        |)
        |
        |func main() {
        |  firstName := "ankit"
        |  log.Fatal("This is the example fatal")
        |  log.Print("This is the example print")
        |}
        |""".stripMargin
    )

    testClassMap.toMap
  }

  "getProbableSinkForGolang" should {
    "return correct list of probable sinks for a valid repository path" in {
      val sinks         = new ProbableSinkExporter(cpg, ruleCache, inputDir.toString)
      val probableSinks = sinks.getProbableSinks
      probableSinks should contain("log.Print")
      probableSinks should not contain ("log.Fatal")
      probableSinks should not contain ("<operator>.assignment")
    }
  }
}
