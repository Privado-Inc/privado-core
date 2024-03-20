package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*
import ai.privado.tagger.utility.APITaggerUtility.{
  getLiteralCode,
  resolveDomainFromSource,
  tagAPIWithDomainAndUpdateRuleCache
}
import ai.privado.utility.Utilities.{addRuleTags, getDomainFromString, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes.AstNode

class JavaAPISinkEndpointMapperByNonInitMethod(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[String](cpg) {

  private val methodFullNameSplitter = "[:(]"

  private val apiMatchingRegex =
    ruleCache.getAllRuleInfo.filter(_.nodeType == NodeType.API).map(_.combinedRulePattern).mkString("(", "|", ")")

  private val thirdPartyRuleInfo = ruleCache.getRuleInfo(Constants.thirdPartiesAPIRuleId)
  override def generateParts(): Array[String] = {

    if (thirdPartyRuleInfo.isDefined) {
      cpg.call
        .where(_.tag.nameExact(InternalTag.API_SINK_MARKED.toString))
        .methodFullName
        .map(_.split(methodFullNameSplitter).headOption.getOrElse(""))
        .filter(_.nonEmpty)
        .map { methodNamespace =>
          val parts = methodNamespace.split("[.]")
          if parts.nonEmpty then parts.dropRight(1).mkString(".") else ""
        }
        .dedup
        .toArray
    } else
      Array[String]()
  }

  override def runOnPart(builder: DiffGraphBuilder, typeFullName: String): Unit = {

    cpg.method.signature(s"$typeFullName$methodFullNameSplitter.*").foreach { clientReturningMethod =>
      val matchingProperties = clientReturningMethod.ast.originalProperty.value(apiMatchingRegex).dedup.l

      val impactedApiCalls = cpg.call
        .methodFullName(s"$typeFullName.*")
        .where(_.tag.nameExact(InternalTag.API_SINK_MARKED.toString))
        .l

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
            storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
          }
        }
      } else { // There is no property node available to be used, try with parameter
        val variableRegex      = ruleCache.getSystemConfigByKey(Constants.apiIdentifier)
        val matchingParameters = clientReturningMethod.parameter.name(variableRegex).l

        if (matchingParameters.nonEmpty) {
          matchingParameters.foreach { parameter =>
            val domain = resolveDomainFromSource(parameter)
            impactedApiCalls.foreach { apiCall =>
              tagAPIWithDomainAndUpdateRuleCache(builder, thirdPartyRuleInfo.get, ruleCache, domain, apiCall, parameter)
              storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
            }
          }
        } else { // There is no matching parameter to be used, try with identifier
          val matchingIdentifiers = clientReturningMethod.ast.isIdentifier.name(variableRegex).l
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
                storeForTag(builder, apiCall, ruleCache)(InternalTag.API_URL_MARKED.toString)
              }
            }
          }
        }
      }
    }
  }
}
