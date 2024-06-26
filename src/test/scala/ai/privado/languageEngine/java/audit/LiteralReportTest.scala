package ai.privado.languageEngine.java.audit

import ai.privado.audit.LiteralReport
import ai.privado.languageEngine.java.tagger.source.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable
import scala.util.Try

class LiteralReportTest extends LiteralReportTestBase {
  override val javaFileContentMap: Map[String, String] = getContent()

  override def beforeAll(): Unit = {
    super.beforeAll()
    SourceTagger.runTagger(cpg, ruleCache, taggerCache)
  }

  def getContent(): Map[String, String] = {
    val testJavaFileMap = mutable.HashMap[String, String]()
    testJavaFileMap.put(
      "main.java",
      """
        |package com.test.privado.audit;
        |
        |public class SendData {
        | public void process() {
        |   String urlValue = "https://www.google.com";
        | }
        |}
        |""".stripMargin
    )
    testJavaFileMap.toMap
  }

  "Test URL sheet" should {
    "should return correct urls" in {
      val workflowResult = LiteralReport.processURLAudit(Try(cpg))

      val urlSet  = mutable.HashSet[String]()
      val lineSet = mutable.HashSet[String]()
      val fileSet = mutable.HashSet[String]()

      workflowResult.foreach(row => {
        urlSet += row.head
        fileSet += row(1)
        lineSet += row(2)
      })

      urlSet should contain("\"https://www.google.com\"")
      fileSet should contain("main.java")
      lineSet should contain("6")
    }
  }

}
