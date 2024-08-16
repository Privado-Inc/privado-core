package ai.privado.entrypoint

import ai.privado.model.{ConfigAndRules, FilterProperty, RuleInfo}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer

object DynamicRuleMerger {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def mergeDynamicRuleSinkForDependencyDiscovery(
    externalSinkRules: List[RuleInfo],
    internalSinkRules: List[RuleInfo]
  ): List[RuleInfo] = {
    try {
      val updatedRules = ListBuffer.from(internalSinkRules)

      externalSinkRules.foreach { externalRule =>
        val externalDomain   = externalRule.domains.headOption.get
        val externalRuleName = externalRule.name

        updatedRules.indexWhere { rule =>
          (rule.domains.contains(externalDomain) || rule.name == externalRuleName) && rule.id.contains(
            "ThirdParties.SDK"
          ) && rule.filterProperty != FilterProperty.CODE
        } match {
          case index if index >= 0 =>
            val matchingRuleInfo = updatedRules(index)
            val updatedRule      = matchingRuleInfo.copy(patterns = matchingRuleInfo.patterns ++ externalRule.patterns)
            updatedRules.update(index, updatedRule)
          case _ =>
            updatedRules += externalRule
        }
      }
      updatedRules.toList
    } catch {
      case e: Exception =>
        logger.error("Error while merging dynamic rules")
        logger.debug("Error occurred ", e)
        externalSinkRules ++ internalSinkRules
    }
  }
}
