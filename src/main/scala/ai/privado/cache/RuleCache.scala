package ai.privado.cache

import ai.privado.model.RuleInfo

import scala.collection.mutable

object RuleCache {

  private val ruleInfoMap = mutable.HashMap[String, RuleInfo]()

  def setRuleInfo(ruleInfo: RuleInfo): Unit = {
    ruleInfoMap.addOne(ruleInfo.id -> ruleInfo)
  }

  def getRuleInfo(ruleId: String): Option[RuleInfo] = {
    ruleInfoMap.get(ruleId)
  }

  def getAllRules(): Seq[RuleInfo] = {
    ruleInfoMap.values.toList
  }
}
