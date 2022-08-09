package ai.privado.cache

import ai.privado.model.{ConfigAndRules, PolicyOrThreat, RuleInfo}

import scala.collection.mutable

/** Cache to store Rules specific things
  */
object RuleCache {

  private var rule: ConfigAndRules = ConfigAndRules(List(), List(), List(), List(), List(), List())
  private val ruleInfoMap          = mutable.HashMap[String, RuleInfo]()
  private val policyOrThreatMap    = mutable.HashMap[String, PolicyOrThreat]()

  def setRule(rule: ConfigAndRules): Unit = {
    this.rule = rule
    rule.sources.foreach(this.setRuleInfo)
    rule.sinks.foreach(this.setRuleInfo)
    rule.collections.foreach(this.setRuleInfo)
    rule.policies.foreach(this.setPolicyOrThreat)
    rule.threats.foreach(this.setPolicyOrThreat)
  }

  def getRule: ConfigAndRules = rule

  private def setRuleInfo(ruleInfo: RuleInfo): Unit = ruleInfoMap.addOne(ruleInfo.id -> ruleInfo)

  def getRuleInfo(ruleId: String): Option[RuleInfo] = ruleInfoMap.get(ruleId)

  def getAllRuleInfo: Seq[RuleInfo] = ruleInfoMap.values.toList

  private def setPolicyOrThreat(policy: PolicyOrThreat): Unit = policyOrThreatMap.addOne(policy.id -> policy)

  def getPolicyOrThreat(policyId: String): Option[PolicyOrThreat] = policyOrThreatMap.get(policyId)

  def getAllPolicyOrThreat: Seq[PolicyOrThreat] = policyOrThreatMap.values.toList

  def getAllPolicy = this.rule.policies

  def getAllThreat = this.rule.threats
}
