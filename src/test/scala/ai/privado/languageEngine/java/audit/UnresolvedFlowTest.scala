package ai.privado.languageEngine.java.audit

import ai.privado.audit.UnresolvedFlowReport
import ai.privado.cache.AuditCache
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.audit.TestData.AuditTestClassData
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator.getSemantics
import ai.privado.languageEngine.java.tagger.source.IdentifierTagger
import ai.privado.utility.Utilities
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import scala.collection.mutable

class UnresolvedFlowTest extends UnresolvedFlowTestBase {
  override val javaFileContentMap: Map[String, String] = getContent()

  override def beforeAll(): Unit = {
    super.beforeAll()
    val privadoInput = PrivadoInput(generateAuditReport = true)
    val context      = new LayerCreatorContext(cpg)
    val options      = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    val sources         = Dataflow.getSources(cpg)
    val unfilteredSinks = UnresolvedFlowReport.getUnresolvedSink(cpg)
    val unresolvedFlows = unfilteredSinks
      .reachableByFlows(sources)(Utilities.getEngineContext(4)(getSemantics(cpg, privadoInput, ruleCache)))
      .l
    AuditCache.setUnfilteredFlow(Dataflow.getExpendedFlowInfo(unresolvedFlows))
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

      val workbookResult = UnresolvedFlowReport.processUnresolvedFlow()

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
      codeSnippet should contain("firstName -> builder() -> firstName(firstName)")
      codeSnippet should contain("firstName -> firstName -> builder() -> firstName(firstName)")
    }
  }
}
