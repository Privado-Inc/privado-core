/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.cache

import ai.privado.model.{ConfigAndRules, Constants, PolicyOrThreat, RuleInfo}

import scala.collection.mutable

/** Cache to store Rules specific things
  */

class RuleCache {
  private var rule: ConfigAndRules =
    ConfigAndRules(List(), List(), List(), List(), List(), List(), List(), List(), List(), List())
  private val ruleInfoMap       = mutable.HashMap[String, RuleInfo]()
  private val policyOrThreatMap = mutable.HashMap[String, PolicyOrThreat]()
  val internalRules             = mutable.HashMap[String, Int]()
  val internalPolicies          = mutable.Set[String]()
  private val storageRuleInfo   = mutable.ListBuffer[RuleInfo]()

  def setRule(rule: ConfigAndRules): Unit = {
    this.rule = rule
    rule.sources.foreach(this.setRuleInfo)
    rule.sinks.foreach(this.setRuleInfo)
    rule.collections.foreach(this.setRuleInfo)
    rule.policies.foreach(this.setPolicyOrThreat)
    rule.threats.foreach(this.setPolicyOrThreat)
  }

  def getRule: ConfigAndRules = rule

  def setRuleInfo(ruleInfo: RuleInfo): Unit = ruleInfoMap.addOne(ruleInfo.id -> ruleInfo)

  def addStorageRuleInfo(ruleInfo: RuleInfo): Unit = storageRuleInfo.addOne(ruleInfo)

  def getStorageRuleInfo(): List[RuleInfo] = storageRuleInfo.toList

  def getRuleInfo(ruleId: String): Option[RuleInfo] = ruleInfoMap.get(ruleId)

  def getAllRuleInfo: Seq[RuleInfo] = ruleInfoMap.values.toList

  private def setPolicyOrThreat(policy: PolicyOrThreat): Unit = policyOrThreatMap.addOne(policy.id -> policy)

  def getPolicyOrThreat(policyId: String): Option[PolicyOrThreat] = policyOrThreatMap.get(policyId)

  def getAllPolicyOrThreat: Seq[PolicyOrThreat] = policyOrThreatMap.values.toList

  def getAllPolicy = this.rule.policies

  def getAllThreat = this.rule.threats

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
    for (rule <- rules.sinkSkipList) {
      internalRules.addOne((rule.id, 0))
    }
  }

  def getSystemConfigByKey(key: String, raw: Boolean = false): String = {
    if (rule.systemConfig.exists(config => config.key.equals(key))) {
      val valueList = rule.systemConfig.filter(config => config.key.equals(key)).map(config => config.value)
      if (raw)
        valueList.mkString("")
      else
        valueList.mkString("(?i)(", "|", ")")
    } else {
      key match {
        case Constants.ignoredSinks =>
          "(?i).*(?<=map|list|jsonobject|json|array|arrays|jsonnode|objectmapper|objectnode).*(put:|get:).*"
        case Constants.apiSinks =>
          "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|fetch|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|" +
            "sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity)"
        case Constants.apiHttpLibraries =>
          "^(?i)(org.apache.http|okhttp|org.glassfish.jersey|com.mashape.unirest|java.net.http|java.net.URL|org.springframework.(web|core.io)|groovyx.net.http|org.asynchttpclient|kong.unirest.java|org.concordion.cubano.driver.http|javax.net.ssl).*"
        case _ => ""
      }
    }
  }
}
