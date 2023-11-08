package ai.privado.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.model.RuleInfo
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.sink.CustomTaggerUtility
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

abstract class CustomAnnotationTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val typeDeclNodeL1 = CustomTaggerUtility.getImpactedTypeDeclNodeByAnnotation(cpg, ruleInfo.patterns.head)
    // We are calling the function getImpactedTypeDeclNode again to get classes following below format
    // @Dao
    // public interface BillableUsageRepository
    // EnrichmentRepository extends BillableUsageRepository
    val typeDeclNode = typeDeclNodeL1 ++ CustomTaggerUtility.getImpactedTypeDeclNodeByExtends(
      cpg,
      typeDeclNodeL1.fullName.mkString("(", "|", ")")
    )
    if (typeDeclNode.nonEmpty) {
      typeDeclNode.fullName.dedup.foreach(typeDeclName => {
        val callNodes = cpg.call.methodFullName(typeDeclName + ".*" + ruleInfo.patterns(1)).l
        callNodes.foreach(callNode => addRuleTags(builder, callNode, ruleInfo, ruleCache))
      })
    }
  }
}
