package ai.privado.languageEngine.ruby.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.entrypoint.ScanProcessor
import ai.privado.languageEngine.java.language.{NodeStarters, StepsForProperty}
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.metric.MetricHandler
import ai.privado.model.{Constants, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.utility.APITaggerUtility.sinkTagger
import io.circe.Json
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.util.Calendar

class APITagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  val cacheCall      = cpg.call.where(_.nameNot("(<operator|<init).*")).l

  lazy val APISINKS_REGEX = ruleCache.getSystemConfigByKey(Constants.apiSinks)

  val apis = cacheCall.name(APISINKS_REGEX).l

  MetricHandler.metricsData("apiTaggerVersion") = Json.fromString("Common HTTP Libraries Used")
  val expanLimit =
    if ScanProcessor.config.limitArgExpansionDataflows > -1 then ScanProcessor.config.limitArgExpansionDataflows else 50
  implicit val engineContext: EngineContext =
    EngineContext(
      semantics = JavaSemanticGenerator.getDefaultSemantics,
      config = EngineConfig(maxCallDepth = 4, maxArgsToAllow = expanLimit, maxOutputArgsExpansion = expanLimit)
    )
  val commonHttpPackages: String = ruleCache.getSystemConfigByKey(Constants.apiHttpLibraries)

  override def generateParts(): Array[_ <: AnyRef] = {
    ruleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val apiInternalSources = cpg.literal.code("(?:\"|'){0,1}(" + ruleInfo.combinedRulePattern + ")(?:\"|'){0,1}").l
    val propertySources    = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l
    // Support to use `identifier` in API's
    val identifierRegex = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
    val identifierSource = {
      if (!ruleInfo.id.equals(Constants.internalAPIRuleId))
        cpg.identifier(identifierRegex).l ++ cpg.property.filter(p => p.name matches (identifierRegex)).usedAt.l
      else
        List()
    }

    logger.debug("Using Enhanced API tagger to find API sinks")
    println(s"${Calendar.getInstance().getTime} - --API TAGGER Common HTTP Libraries Used...")
    sinkTagger(
      apiInternalSources ++ propertySources ++ identifierSource,
      apis.methodFullName(commonHttpPackages).l,
      builder,
      ruleInfo,
      ruleCache
    )
  }
}
