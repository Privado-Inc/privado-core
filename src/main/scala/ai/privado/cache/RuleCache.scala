package ai.privado.cache

import ai.privado.model.{ConfigAndRules, PolicyOrThreat, RuleInfo}

import scala.collection.mutable

/** Cache to store Rules specific things
  */
object RuleCache {

  private var rule: ConfigAndRules = ConfigAndRules(List(), List(), List(), List(), List(), List())
  private val ruleInfoMap          = mutable.HashMap[String, RuleInfo]()
  private val policyMap            = mutable.HashMap[String, PolicyOrThreat]()
  val internalRules                = mutable.HashMap[String, Int]()

  def setRule(rule: ConfigAndRules): Unit = {
    this.rule = rule
    rule.sources.foreach(this.setRuleInfo)
    rule.sinks.foreach(this.setRuleInfo)
    rule.collections.foreach(this.setRuleInfo)
    rule.policies.foreach(this.setPolicy)
  }

  def getRule: ConfigAndRules = rule

  private def setRuleInfo(ruleInfo: RuleInfo): Unit = ruleInfoMap.addOne(ruleInfo.id -> ruleInfo)

  def getRuleInfo(ruleId: String): Option[RuleInfo] = ruleInfoMap.get(ruleId)

  def getAllRuleInfo: Seq[RuleInfo] = ruleInfoMap.values.toList

  private def setPolicy(policy: PolicyOrThreat): Unit = policyMap.addOne(policy.id -> policy)

  def getPolicy(policyId: String): Option[PolicyOrThreat] = policyMap.get(policyId)

  def getAllPolicy: Seq[PolicyOrThreat] = policyMap.values.toList

  def setInternalRules(rules: ConfigAndRules) = {
    for (rule <- rules.sinks) {
      internalRules.addOne((rule.id, 0))
    }
    for (rule <- rules.sources) {
      internalRules.addOne((rule.id, 0))
    }
    for (rule <- rules.collections) {
      internalRules.addOne((rule.id, 0))
    }
    for (rule <- rules.policies) {
      internalRules.addOne((rule.id, 0))
    }
    for (rule <- rules.exclusions) {
      internalRules.addOne((rule.id, 0))
    }
  }
}
