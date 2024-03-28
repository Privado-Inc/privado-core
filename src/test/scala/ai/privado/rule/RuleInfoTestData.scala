package ai.privado.rule

import ai.privado.model.*
import ai.privado.rule.SourceRuleTestData._
import ai.privado.rule.SinkRuleTestData._

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
}
