package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import ai.privado.tagger.utility.APITaggerUtility.{resolveDomainFromSource, tagAPIWithDomainAndUpdateRuleCache}
import ai.privado.utility.Utilities.{getDomainFromString, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}

import scala.util.{Failure, Success, Try}

object Utility {

  def tagAPICallByItsUrlMethod(
    cpg: Cpg,
    builder: DiffGraphBuilder,
    methodNode: Method,
    apiCalls: List[Call],
    apiMatchingRegex: String,
    thirdPartyRuleInfo: Option[RuleInfo],
    ruleCache: RuleCache
  ): Unit = {

    val impactedApiCalls = apiCalls.whereNot(_.tag.nameExact(InternalTag.API_URL_MARKED.toString)).l

    if (impactedApiCalls.nonEmpty) {
      val matchingProperties = methodNode.ast.originalProperty.value(apiMatchingRegex).dedup.l
      if (matchingProperties.nonEmpty) {
        matchingProperties.foreach { propertyNode =>
          val domain = getDomainFromString(propertyNode.value)
          impactedApiCalls.foreach { apiCall =>
            tagAPIWithDomainAndUpdateRuleCache(
              builder,
              thirdPartyRuleInfo.get,
              ruleCache,
              domain,
              apiCall,
              propertyNode
            )
            storeForTag(builder, apiCall, ruleCache)(InternalTag.API_SINK_MARKED.toString)
            storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
          }
        }
      } else {
        // Try fetching the url from the injection happening via Named annotation
        val endpointNode = Try {
          val namedCode = methodNode.parameter.annotation.name("Named").parameterAssign.code.headOption.getOrElse("")
          val fieldAccessNode = cpg
            .call("named")
            .whereNot(_.file.name(".*Mock.*"))
            .where(_.argument.code(namedCode))
            .inCall
            .inCall
            .argument
            .isCall
            .name(Operators.fieldAccess)
            .l
          val endpointMemberCode = fieldAccessNode.code.head.split("\\$").last
          val endpointNode = {
            val lastArg = fieldAccessNode
              .repeat(_.astParent)(_.until(_.filter(_.isTypeDecl)))
              .ast
              .isCall
              .name(Operators.fieldAccess)
              .inCall
              .name(Operators.assignment)
              .or(
                _.filter(_.argument.order(1).code(s".*$endpointMemberCode").nonEmpty),
                _.filter(_.argument.order(1).code.filter(endpointMemberCode.endsWith).nonEmpty)
              )
              .argument
              .last
            if (lastArg.originalProperty.isDefined)
              lastArg.originalProperty.head
            else if (lastArg.isLiteral)
              lastArg
            else
              throw new UnsupportedOperationException
          }
          endpointNode
        }.toOption
        if (endpointNode.isDefined) {
          val domain = resolveDomainFromSource(endpointNode.get)
          impactedApiCalls.foreach { apiCall =>
            tagAPIWithDomainAndUpdateRuleCache(
              builder,
              thirdPartyRuleInfo.get,
              ruleCache,
              domain,
              apiCall,
              endpointNode.get
            )
            storeForTag(builder, apiCall, ruleCache)(InternalTag.API_SINK_MARKED.toString)
            storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
          }

        } else { // There is no property node available to be used, try with parameter
          val variableRegex      = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
          val matchingParameters = methodNode.parameter.name(variableRegex).l

          if (matchingParameters.nonEmpty) {
            matchingParameters.foreach { parameter =>
              val domain = resolveDomainFromSource(parameter)
              impactedApiCalls.foreach { apiCall =>
                tagAPIWithDomainAndUpdateRuleCache(
                  builder,
                  thirdPartyRuleInfo.get,
                  ruleCache,
                  domain,
                  apiCall,
                  parameter
                )
                storeForTag(builder, apiCall, ruleCache)(InternalTag.API_SINK_MARKED.toString)
                storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
              }
            }
          } else { // There is no matching parameter to be used, try with identifier
            val matchingIdentifiers = methodNode.ast.isIdentifier.name(variableRegex).l
            if (matchingIdentifiers.nonEmpty) {
              matchingIdentifiers.foreach { identifier =>
                val domain = resolveDomainFromSource(identifier)
                impactedApiCalls.foreach { apiCall =>
                  tagAPIWithDomainAndUpdateRuleCache(
                    builder,
                    thirdPartyRuleInfo.get,
                    ruleCache,
                    domain,
                    apiCall,
                    identifier
                  )
                  storeForTag(builder, apiCall, ruleCache)(InternalTag.API_SINK_MARKED.toString)
                  storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
                }
              }
            }
          }
        }
      }
    }
  }

}
