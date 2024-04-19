package ai.privado.rule

import ai.privado.cache.RuleCache
import ai.privado.model.*
import ai.privado.rule.SourceRuleTestData.*
import ai.privado.rule.SinkRuleTestData.*

object RuleInfoTestData {

  val sourceRule = List(
    firstNameSourceRule,
    accountPasswordSourceRule,
    lastNameSourceRule,
    dobSourceRule,
    emailSourceRule,
    phoneNumberSourceRule,
    salarySourceRule
  )

  val sinkRule = List(thirdPartyAPIRule)

  val rule: ConfigAndRules =
    ConfigAndRules(sources = sourceRule, sinks = sinkRule)

  val ruleCache: RuleCache = {
    val r = RuleCache()
    r.setRule(rule)
    r
  }
}
