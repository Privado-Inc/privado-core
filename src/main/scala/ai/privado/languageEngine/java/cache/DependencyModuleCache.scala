package ai.privado.languageEngine.java.cache

import ai.privado.audit.AuditReportConstants
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
      AuditReportConstants.DEPENDENCY_UNKNOWN_LIBRARY_NAME
    }
  }

  def getDependencyRuleIfExist(moduleDependency: ModuleDependency): String = {
    if (dependencyRuleSet.exists(_.moduleDependency == moduleDependency)) {
      dependencyRuleSet.filter(_.moduleDependency == moduleDependency).head.ruleInfo.id
    } else {
      AuditReportConstants.AUDIT_EMPTY_CELL_VALUE
    }
  }
}
