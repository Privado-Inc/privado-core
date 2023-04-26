package ai.privado.languageEngine.java.cache

import ai.privado.model.RuleInfo
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency

import scala.collection.mutable

object DependencyModuleCache {

  private val dependencyRuleSet: mutable.HashSet[DependencyRuleInfo] = new mutable.HashSet[DependencyRuleInfo]()

  case class DependencyRuleInfo(moduleDependency: ModuleDependency, ruleInfo: RuleInfo, category: String)

  def addIntoDependencyRule(dependencyRule: DependencyRuleInfo): Unit = dependencyRuleSet += dependencyRule

  def checkIfDependencyRuleExist(moduleDependency: ModuleDependency): String = {
    if (dependencyRuleSet.exists(_.moduleDependency == moduleDependency)) {
      dependencyRuleSet.filter(_.moduleDependency == moduleDependency).head.category
    } else {
      "NA"
    }
  }

  def getDependencyRuleIfExist(moduleDependency: ModuleDependency): String = {
    if (dependencyRuleSet.exists(_.moduleDependency == moduleDependency)) {
      dependencyRuleSet.filter(_.moduleDependency == moduleDependency).head.ruleInfo.id
    } else {
      "--"
    }
  }

  def check() = {
    println(dependencyRuleSet.size)
  }
}
