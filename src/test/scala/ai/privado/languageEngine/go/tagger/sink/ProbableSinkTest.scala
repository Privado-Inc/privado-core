package ai.privado.languageEngine.go.tagger.sink

import io.shiftleft.semanticcpg.language.*
import ai.privado.exporter.ProbableSinkExporter
import ai.privado.languageEngine.go.GoTestBase
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
        |  log.Fatal("This is the example fatal")
        |  log.Print("This is the example print")
        |}
        |""".stripMargin
    )

    testClassMap.toMap
  }

  "getProbableSinkForGolang" should {
    "return correct list of probable sinks for a valid repository path" in {
      val sinks = new ProbableSinkExporter(cpg, ruleCache, inputDir.toString)
      val paths = sinks.getProbableSinks
      paths.size shouldBe 1
      paths.head shouldBe "log.Print"
    }
  }
}
