package ai.privado.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.semantic.language.*
import ai.privado.model.FilterProperty.*
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.tagger.utility.APITaggerUtility.tagThirdPartyAPIWithDomainAndUpdateRuleCache
import ai.privado.utility.Utilities.{getDomainFromString, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.util.{Try, Success, Failure}

/** Read the inference rule defined in passed ruleCache, and tag the API Sink and the corresponding API Endpoint
  * @param cpg
  * @param ruleCache
  */
class InferenceAPIEndpointTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[RuleInfo] =
    Try(ruleCache.getRule.inferences.filter(_.catLevelTwo.equals(Constants.apiEndpoint)).toArray) match
      case Success(rules) => rules
      case Failure(exception: Exception) => {
        logger.error("Cannot generateParts for InferenceAPIEndpointTagger: ", exception.getMessage)
        Array.empty[RuleInfo]
      }
      case Failure(_) => {
        logger.error("Cannot generateParts for InferenceAPIEndpointTagger")
        Array.empty[RuleInfo]
      }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    try {
      val domain = ruleInfo.domains.headOption.getOrElse("")

      ruleInfo.filterProperty match {
        case METHOD_FULL_NAME_WITH_LITERAL =>
          val apiSinks = cpg.call
            .or(
              _.methodFullName(ruleInfo.combinedRulePattern),
              _.filter(_.dynamicTypeHintFullName.exists(_.matches(ruleInfo.combinedRulePattern)))
            )
            .l

          apiSinks.foreach { apiCall => tagNode(builder, apiCall, domain) }

        case METHOD_FULL_NAME_WITH_PROPERTY_NAME =>
          val apiUrlFromProperty = cpg.property.name(domain).value.headOption

          if (apiUrlFromProperty.isDefined) {
            val apiSinks = cpg.call
              .or(
                _.methodFullName(ruleInfo.combinedRulePattern),
                _.filter(_.dynamicTypeHintFullName.exists(_.matches(ruleInfo.combinedRulePattern)))
              )
              .l

            apiSinks.foreach { apiCall => tagNode(builder, apiCall, apiUrlFromProperty.get) }
          }

        case _ =>
      }
    } catch {
      case ex => logger.error("Error while executing InferenceApiEndpointTagger runOnPart: ", ex.getMessage)
      case _  => logger.error("Cannot runOnPart for InferenceApiEndpointTagger")
    }
  }

  private def tagNode(builder: DiffGraphBuilder, apiCall: AstNode, apiUrl: String) = {
    tagThirdPartyAPIWithDomainAndUpdateRuleCache(builder, cpg, ruleCache, getDomainFromString(apiUrl), apiCall, apiUrl)
    storeForTag(builder, apiCall, ruleCache)(InternalTag.API_SINK_MARKED.toString)
    storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
  }
}
