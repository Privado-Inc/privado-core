package ai.privado.languageEngine.ruby.tagger.sink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.language.{NodeStarters, StepsForProperty}
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.metric.MetricHandler
import ai.privado.model.{Constants, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.utility.APITaggerUtility.sinkTagger
import ai.privado.utility.Utilities
import io.circe.Json
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala
import java.util.Calendar

class APITagger(cpg: Cpg, ruleCache: RuleCache, privadoInput: PrivadoInput, appCache: AppCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger        = LoggerFactory.getLogger(this.getClass)
  val cacheCall: List[Call] = cpg.call.where(_.nameNot(Operators.ALL.asScala.toSeq: _*)).l

  val APISINKS_REGEX: String = ruleCache.getSystemConfigByKey(Constants.apiSinks)

  val apis: List[Call] = cacheCall.name(APISINKS_REGEX).l

  MetricHandler.metricsData("apiTaggerVersion") = Json.fromString("Common HTTP Libraries Used")
  implicit val engineContext: EngineContext = Utilities.getEngineContext(privadoInput, appCache, 4)
  val commonHttpPackages: String            = ruleCache.getSystemConfigByKey(Constants.apiHttpLibraries)

  val httpApis: List[Call] = apis
    .or(_.methodFullName(commonHttpPackages), _.filter(_.dynamicTypeHintFullName.exists(_.matches(commonHttpPackages))))
    .l

  val clientLikeApis: List[Call] = cacheCall
    .code("(?i).*(client|connection).*[.](get|post|delete|put|patch).*")
    .name("get|post|post_json|delete|put|patch")
    .l

  // Support to use `identifier` in API's
  val identifierRegex: String = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)

  override def generateParts(): Array[_ <: AnyRef] = {
    ruleCache.getRule.sinks
      .filter(rule => rule.nodeType.equals(NodeType.API))
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val apiInternalSources = cpg.literal.code("(?:\"|'){0,1}(" + ruleInfo.combinedRulePattern + ")(?:\"|'){0,1}").l
    val propertySources    = cpg.property.filter(p => p.value matches (ruleInfo.combinedRulePattern)).usedAt.l

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
      (httpApis ++ clientLikeApis).distinct,
      builder,
      ruleInfo,
      ruleCache,
      privadoInput
    )
  }
}
