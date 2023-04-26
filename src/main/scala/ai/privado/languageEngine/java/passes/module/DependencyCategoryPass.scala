package ai.privado.languageEngine.java.passes.module

import ai.privado.cache.RuleCache
import ai.privado.model.RuleInfo
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency
import io.shiftleft.passes.ForkJoinParallelCpgPass
import ai.privado.languageEngine.java.cache.DependencyModuleCache
import ai.privado.languageEngine.java.cache.DependencyModuleCache.DependencyRuleInfo

class DependencyCategoryPass(cpg: Cpg, ruleCache: RuleCache, moduleDependencies: List[ModuleDependency])
    extends ForkJoinParallelCpgPass[RuleInfo](cpg) {
  override def generateParts(): Array[RuleInfo] = ruleCache.getRule.sinks.toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {

    val rulePattern = ruleInfo.combinedRulePattern
    moduleDependencies.foreach(moduleDependency => {
      if (moduleDependency.groupid.matches(rulePattern)) {
        DependencyModuleCache.addIntoDependencyRule(
          DependencyRuleInfo(moduleDependency, ruleInfo, ruleInfo.catLevelTwo)
        )
      }
    })
  }
}
