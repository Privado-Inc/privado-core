package ai.privado.languageEngine.javascript.audit

import ai.privado.audit.APIReport
import ai.privado.entrypoint.PrivadoInput
import ai.privado.audit.LiteralReport
import ai.privado.cache.{AppCache, FileLinkingMetadata}
import ai.privado.languageEngine.javascript.tagger.sink.JSAPITagger
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger

import scala.collection.mutable
import scala.util.Try

class HTTPReportTest extends HTTPReportTestBase {

  override val javascriptFileContentMap: Map[String, String] = getContent()

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new JSAPITagger(cpg, ruleCache, PrivadoInput(), new AppCache(), fileLinkingMetadata = FileLinkingMetadata())
  }

  def getContent(): Map[String, String] = {
    val testJavaScriptFileMap = mutable.HashMap[String, String]()
    testJavaScriptFileMap.put(
      "main.js",
      """
        |function main() {
        |    const randomVar = "https://www.example.com";
        |}
        |""".stripMargin
    )

    testJavaScriptFileMap.toMap
  }

  "Test HTTP sheet" should {
    "should return correct http literal" in {
      val workflowResult = LiteralReport.processHTTPAudit(Try(cpg))

      val codeSet = mutable.HashSet[String]()
      val lineSet = mutable.HashSet[String]()
      val fileSet = mutable.HashSet[String]()

      workflowResult.foreach(row => {
        codeSet += row.head
        fileSet += row(1)
        lineSet += row(2)
      })

      workflowResult.size shouldBe 2
      codeSet should contain("\"https://www.example.com\"")
      fileSet should contain("main.js")
      lineSet should contain("3")
    }
  }
}
