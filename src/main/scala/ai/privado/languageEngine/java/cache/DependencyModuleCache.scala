package ai.privado.languageEngine.java.cache

import ai.privado.audit.AuditReportConstants
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency

import scala.collection.mutable

object DependencyModuleCache {
  case class RuleCategoryInfo(rule: String, category: String)

  private val dependencyRuleMap: mutable.HashMap[ModuleDependency, mutable.HashSet[RuleCategoryInfo]] = new mutable.HashMap[ModuleDependency, mutable.HashSet[RuleCategoryInfo]]()

  def addIntoDependencyRule(moduleDependency: ModuleDependency, ruleInfo: String, category: String): Unit = {
    if (dependencyRuleMap.contains(moduleDependency)) {
      dependencyRuleMap(moduleDependency) += RuleCategoryInfo(ruleInfo, category)
    } else {
      dependencyRuleMap.put(moduleDependency, mutable.HashSet(RuleCategoryInfo(ruleInfo, category)))
    }
  }
  def getRuleCategoryInfo(moduleDependency: ModuleDependency): Set[RuleCategoryInfo] = {
    if (dependencyRuleMap.contains(moduleDependency)) {
      dependencyRuleMap(moduleDependency).toSet
    } else {
      Set(RuleCategoryInfo(AuditReportConstants.AUDIT_EMPTY_CELL_VALUE, AuditReportConstants.DEPENDENCY_UNKNOWN_LIBRARY_NAME))
    }
  }
}
