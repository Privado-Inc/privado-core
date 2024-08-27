package ai.privado.inputprocessor

import ai.privado.cache.RuleCache
import ai.privado.model.{ConfigAndRules, FilterProperty, RuleInfo}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map}

trait DynamicRuleMerger {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def mergeDynamicRuleSinkForDependencyDiscovery(
    externalSinkRules: List[RuleInfo],
    internalSinkRules: List[RuleInfo],
    ruleCache: RuleCache
  ): List[RuleInfo] = {
    try {

      val externalOtherRule = new ListBuffer[RuleInfo]

      val internalRuleMap = mutable.Map(
        internalSinkRules
          .filter(rule => rule.domains.nonEmpty && rule.name.nonEmpty)
          .map(rule => ((rule.domains.headOption.get, rule.name, rule.filterProperty), rule)): _*
      )

      externalSinkRules.foreach { externalRule =>
        if (externalRule.tags.contains("Discovery_Generated")) {
          val externalId             = externalRule.id
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
              ruleCache.addIntoMergedDynamicRuleMapper(externalId, matchingRule.id)
            case _ =>
              internalRuleMap((externalDomain, externalRuleName, externalFilterProperty)) = externalRule
              ruleCache.addIntoMergedDynamicRuleMapper(externalRuleName, externalRuleName)
        } else {
          externalOtherRule.append(externalRule)
        }
      }

      internalRuleMap.values.toList ++ externalOtherRule.toList
    } catch {
      case e: Exception =>
        logger.error("Error while merging dynamic rules")
        logger.debug("Error occurred ", e)
        externalSinkRules ++ internalSinkRules
    }
  }
}
