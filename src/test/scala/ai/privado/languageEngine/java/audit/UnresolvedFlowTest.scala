package ai.privado.languageEngine.java.audit

import ai.privado.audit.UnresolvedFlowReport
import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.dataflow.Dataflow
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator.getSemantics
import ai.privado.languageEngine.java.tagger.source.*
import ai.privado.model.Language
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.collection.mutable

class UnresolvedFlowTest extends UnresolvedFlowTestBase {
  override val javaFileContentMap: Map[String, String] = getContent()

  override def beforeAll(): Unit = {
    super.beforeAll()
    val appCache = new AppCache()
    appCache.repoLanguage = Language.JAVA

    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    SourceTagger.runTagger(cpg, ruleCache, taggerCache)
    val sources         = Dataflow.getSources(cpg)
    val unfilteredSinks = UnresolvedFlowReport.getUnresolvedSink(cpg)
    val unresolvedFlows = unfilteredSinks
      .reachableByFlows(sources)(
        Utilities.getEngineContext(config = privadoInput, appCache, 4)(getSemantics(cpg, privadoInput, ruleCache))
      )
      .l
    auditCache.setUnfilteredFlow(Dataflow.getExpendedFlowInfo(unresolvedFlows, appCache, new RuleCache()))
  }

  def getContent(): Map[String, String] = {
    val testJavaFileMap = mutable.HashMap[String, String]()
    testJavaFileMap.put("project/UnresolvedBaseClass.java", AuditTestClassData.unresolvedBaseClass)
    testJavaFileMap.toMap
  }

  "Unresolved Flow" should {
    "Test Unresolved List" in {
      val sourceSet   = mutable.HashSet[String]()
      val sinkSet     = mutable.HashSet[String]()
      val codeSnippet = mutable.HashSet[String]()

      val workbookResult = UnresolvedFlowReport.processUnresolvedFlow(auditCache)

      workbookResult.foreach(row => {
        sourceSet += row.head
        sinkSet += row(1)
        codeSnippet += row(3)
      })

      workbookResult.size shouldBe 3

      // Test source Info
      sourceSet should contain("Data.Sensitive.FirstName")

      // Test sink info
      sinkSet should contain("<unresolvedNamespace>.firstName:<unresolvedSignature>(1)")

      // Test code snippet info
      codeSnippet should contain("firstName -> builder() -> builder().firstName(firstName)")
      codeSnippet should contain("firstName -> firstName -> builder() -> builder().firstName(firstName)")
    }
  }
}
