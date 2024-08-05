package ai.privado.languageEngine.c.tagger.source

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.model.{InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.semanticcpg.language.*

class IdentifierTagger(cpg: Cpg, ruleCache: RuleCache, taggerCache: TaggerCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val rulePattern = ruleInfo.combinedRulePattern
    val regexMatchingIdentifiers =
      cpg.identifier(rulePattern).filterNot(item => item.name.equals(item.name.toUpperCase))

    regexMatchingIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo, ruleCache)
    })

    val regexMatchingMembers = cpg.member.name(rulePattern).l

    regexMatchingMembers.foreach(member => {
      storeForTag(builder, member, ruleCache)(InternalTag.VARIABLE_REGEX_MEMBER.toString)
      addRuleTags(builder, member, ruleInfo, ruleCache)
    })

    val regexMatchingFieldIdentifiersIdentifiers =
      cpg.fieldAccess.where(_.fieldIdentifier.canonicalName(rulePattern)).isCall.l

    regexMatchingFieldIdentifiersIdentifiers.foreach(identifier => {
      storeForTag(builder, identifier, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
      addRuleTags(builder, identifier, ruleInfo, ruleCache)
    })
  }

}
