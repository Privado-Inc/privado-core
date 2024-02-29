package ai.privado.languageEngine.ruby.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.model.{InternalTag, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class RubyLiteralTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  private val cachedColonLiteral = cpg.literal.filter(_.code.startsWith(":")).l

  override def generateParts(): Array[RuleInfo] = ruleCache.getRule.sources.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val literalStartingWithColon =
      cachedColonLiteral.filter(_.code.stripPrefix(":").matches(ruleInfo.combinedRulePattern)).l

    literalStartingWithColon.foreach(literal => {
      storeForTag(builder, literal, ruleCache)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
      addRuleTags(builder, literal, ruleInfo, ruleCache)
    })

  }

}
