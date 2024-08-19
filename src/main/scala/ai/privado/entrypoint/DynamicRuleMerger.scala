package ai.privado.entrypoint

import ai.privado.model.{ConfigAndRules, FilterProperty, RuleInfo}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

trait DynamicRuleMerger {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def mergeDynamicRuleSinkForDependencyDiscovery(
    externalSinkRules: List[RuleInfo],
    internalSinkRules: List[RuleInfo]
  ): List[RuleInfo] = {
    try {

      val internalRuleMap = mutable.Map(
        internalSinkRules.map(rule => ((rule.domains.headOption.get, rule.name, rule.filterProperty), rule))*
      )

      externalSinkRules.foreach { externalRule =>
        val externalDomain         = externalRule.domains.headOption.get
        val externalRuleName       = externalRule.name
        val externalFilterProperty = externalRule.filterProperty

        internalRuleMap.collectFirst {
          case ((domain, name, filterProperty), rule)
              if (domain == externalDomain || name == externalRuleName) && rule.id.contains(
                "ThirdParties.SDK"
              ) && filterProperty != FilterProperty.CODE =>
            (domain, name, filterProperty, rule)
        } match
          case Some(_, _, _, matchingRule: RuleInfo) =>
            val updatedRule = matchingRule.copy(patterns = matchingRule.patterns ++ externalRule.patterns)
            internalRuleMap.update(
              (matchingRule.domains.headOption.get, matchingRule.name, matchingRule.filterProperty),
              updatedRule
            )
          case _ =>
            internalRuleMap((externalDomain, externalRuleName, externalFilterProperty)) = externalRule
      }

      internalRuleMap.values.toList
    } catch {
      case e: Exception =>
        logger.error("Error while merging dynamic rules")
        logger.debug("Error occurred ", e)
        externalSinkRules ++ internalSinkRules
    }
  }
}
