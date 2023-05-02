package ai.privado.languageEngine.java.passes.module

import ai.privado.audit.AuditReportConstants
import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency
import io.shiftleft.passes.ForkJoinParallelCpgPass
import ai.privado.languageEngine.java.cache.DependencyModuleCache
import ai.privado.languageEngine.java.cache.DependencyModuleCache.DependencyRuleInfo

class DependencyCategoryPass(cpg: Cpg, ruleCache: RuleCache, moduleDependencies: List[ModuleDependency])
    extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  override def generateParts(): Array[RuleInfo] = getAuditAllRules(ruleCache)

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val rulePattern = ruleInfo.combinedRulePattern
    moduleDependencies.foreach(moduleDependency => {
      if (moduleDependency.groupid.matches(rulePattern)) {
        DependencyModuleCache.addIntoDependencyRule(
          DependencyRuleInfo(moduleDependency, ruleInfo, getCategoryName(ruleInfo))
        )
      }
    })
  }

  private def getAuditAllRules(ruleCache: RuleCache): Array[RuleInfo] = {
    (ruleCache.getRule.sinks ++ ruleCache.getRule.auditConfig).toArray
  }

  private def getCategoryName(ruleInfo: RuleInfo): String = {
    ruleInfo.catLevelTwo match {
      case Constants.third_parties   => AuditReportConstants.DEPENDENCY_KNOW_THIRD_PARTY_LIBRARY_NAME
      case Constants.storages        => AuditReportConstants.DEPENDENCY_STORAGE_LIBRARY_NAME
      case Constants.leakages        => AuditReportConstants.DEPENDENCY_LEAKAGE_LIBRARY_NAME
      case Constants.auditCollection => AuditReportConstants.DEPENDENCY_COLLECTION_LIBRARY_NAME
      case Constants.auditWebClient  => AuditReportConstants.DEPENDENCY_WEB_CLIENT_LIBRARY_NAME
      case Constants.auditUtility    => AuditReportConstants.DEPENDENCY_UTILITY_LIBRARY_NAME
      case _                         => AuditReportConstants.DEPENDENCY_UNKNOWN_LIBRARY_NAME
    }
  }
}
