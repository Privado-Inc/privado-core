package ai.privado.cache

import ai.privado.model.{Policy, RuleInfo, Rules}

import scala.collection.mutable

object RuleCache {

  private var rule: Rules = Rules(List(), List(), List(), List())
  private val ruleInfoMap = mutable.HashMap[String, RuleInfo]()
  private val policyMap   = mutable.HashMap[String, Policy]()

  def setRule(rule: Rules): Unit = {
    this.rule = rule
    rule.sources.foreach(this.setRuleInfo)
    rule.sinks.foreach(this.setRuleInfo)
    rule.collections.foreach(this.setRuleInfo)
    rule.policies.foreach(this.setPolicy)
  }

  def getRule: Rules = rule

  private def setRuleInfo(ruleInfo: RuleInfo): Unit = ruleInfoMap.addOne(ruleInfo.id -> ruleInfo)

  def getRuleInfo(ruleId: String): Option[RuleInfo] = ruleInfoMap.get(ruleId)

  def getAllRuleInfo: Seq[RuleInfo] = ruleInfoMap.values.toList

  private def setPolicy(policy: Policy): Unit = policyMap.addOne(policy.id -> policy)

  def getPolicy(policyId: String): Option[Policy] = policyMap.get(policyId)

  def getAllPolicy: Seq[Policy] = policyMap.values.toList
}
