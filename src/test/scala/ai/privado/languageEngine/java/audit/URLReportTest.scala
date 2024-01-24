package ai.privado.languageEngine.java.audit

import scala.collection.mutable
import ai.privado.audit.URLReport
import ai.privado.languageEngine.java.tagger.source.IdentifierTagger
import io.shiftleft.semanticcpg.language.*

import scala.util.Try

class URLReportTest extends URLReportTestBase {
  override val javaFileContentMap: Map[String, String] = getContent()

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
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
      val workflowResult = URLReport.processURLAudit(Try(cpg))

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
