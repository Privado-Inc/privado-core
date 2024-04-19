package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.*
import ai.privado.languageEngine.java.tagger.sink.api.Utility.tagAPICallByItsUrlMethod
import ai.privado.tagger.utility.APITaggerUtility.{
  getLiteralCode,
  resolveDomainFromSource,
  tagAPIWithDomainAndUpdateRuleCache
}
import ai.privado.utility.Utilities.{addRuleTags, getDomainFromString, storeForTag}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, Method}

class JavaAPISinkEndpointMapperByNonInitMethod(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[String](cpg) {

  private val methodFullNameSplitter = "[:(]"

  private val apiMatchingRegex =
    ruleCache.getAllRuleInfo.filter(_.nodeType == NodeType.API).map(_.combinedRulePattern).mkString("(", "|", ")")

  override def generateParts(): Array[String] = {

    /* General assumption - there is a function which creates a client, and the usage of the client and binding
     happens via dependency injection which can very according to the framework used.
     If we identify such a function and can point to the usage of a particular endpoint in it,
     we can say the client uses the following endpoint
     */

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
  }

  override def runOnPart(builder: DiffGraphBuilder, typeFullName: String): Unit = {

    cpg.method.signature(s"$typeFullName$methodFullNameSplitter.*").foreach { clientReturningMethod =>

      val impactedApiCalls = cpg.call
        .methodFullName(s"$typeFullName.*")
        .where(_.tag.nameExact(InternalTag.API_SINK_MARKED.toString))
        .l

      tagAPICallByItsUrlMethod(cpg, builder, clientReturningMethod, impactedApiCalls, apiMatchingRegex, ruleCache)
    }
  }
}
