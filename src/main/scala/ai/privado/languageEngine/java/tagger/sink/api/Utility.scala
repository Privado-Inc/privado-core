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
      /*
      Try if we can get to a node in the methodNode which points to a property node, and matches the api regex on the properties value
       */
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
        /* Try fetching the url from the injection happening via Named annotation
          Looks for parameters marked with @Named annotation, try getting to the binding and resolve the api url,
        If we are able to resolve the api url use that or else, return the matching parameterAssign node's code
         */
        val apiVariableRegex = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
        val endpointNode = Try {
          val namedUrlNode = methodNode.parameter.annotation.name("Named").parameterAssign.code(apiVariableRegex).head
          val fieldAccessNode = cpg
            .call("named")
            .whereNot(_.file.name(".*Mock.*"))
            .where(_.argument.code(namedUrlNode.code))
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
                _.filter(_.argument.order(1).code.exists(endpointMemberCode.endsWith))
              )
              .argument
              .last
            if (lastArg.originalProperty.isDefined) // Return the property node
              lastArg.originalProperty.head
            else if (lastArg.isLiteral) // Being literal point to a url
              lastArg
            else // Return the parameterAssign node, which is the value inside the @Named annotation, Ex- @Name(ConfigKeys.MY_SERVICE_ENDPOINT), returns ConfigKeys.MY_SERVICE_ENDPOINT
              namedUrlNode
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

        } else { // There is no property node available to be used, try matching against the parameter name
          val variableRegex      = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
          val matchingParameters = methodNode.parameter.name(variableRegex).l

          if (matchingParameters.nonEmpty) {
            val parameter =
              matchingParameters.head // Pick only the first parameter as we don't want to tag same sink with multiple API's
            val domain = resolveDomainFromSource(parameter)
            impactedApiCalls.foreach { apiCall =>
              tagAPIWithDomainAndUpdateRuleCache(builder, thirdPartyRuleInfo.get, ruleCache, domain, apiCall, parameter)
              storeForTag(builder, apiCall, ruleCache)(InternalTag.API_SINK_MARKED.toString)
              storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
            }

          } else { // There is no matching parameter to be used,  try matching against the identifier name
            val matchingIdentifiers = methodNode.ast.isIdentifier.name(variableRegex).l
            if (matchingIdentifiers.nonEmpty) {
              val identifier =
                matchingIdentifiers.head // Pick only the first identifier as we don't want to tag same sink with multiple API's
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
