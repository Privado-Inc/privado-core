package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call}
import io.shiftleft.semanticcpg.language.*

protected trait TaggerHelper {

  val flinkSinkName = "addSink"

  /** Get call nodes tagged as flink connectors
    * @param cpg
    * @param ruleCache
    * @return
    */
  def getFlinkDefaultConnectors(cpg: Cpg, ruleCache: RuleCache): List[Call] = {
    val flinkConnectorsRuleIds = ruleCache.getSystemConfigByKey(Constants.flinkConnectorProducerRuleIds)

    val unfilteredConnectors = cpg.call
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
      .l
    // We do the below filtering from Performance POV, as we only need to focus on flink connectors
    if flinkConnectorsRuleIds.isEmpty then unfilteredConnectors
    else unfilteredConnectors.where(_.tag.nameExact(Constants.id).value(flinkConnectorsRuleIds)).l
  }

  /** Helper function to copy tags from nodeFrom to nodeTo
    *
    * @param builder
    * @param ruleCache
    * @param nodeFrom
    * @param nodeTo
    */
  def copyTags(builder: DiffGraphBuilder, ruleCache: RuleCache, nodeFrom: List[AstNode], nodeTo: AstNode): Unit =
    nodeFrom.tag.dedup.foreach(t => storeForTag(builder, nodeTo, ruleCache)(t.name, t.value))

}
