package ai.privado.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.passes.Utility
import ai.privado.tagger.PrivadoSimpleCpgPass
import ai.privado.utility.Utilities
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

class DependencyNodeTagger(cpg: Cpg, dependencies: List[DependencyInfo], ruleCache: RuleCache)
    extends PrivadoSimpleCpgPass(cpg)
    with Utility {
  private val logger = LoggerFactory.getLogger(this.getClass)
  def run(builder: DiffGraphBuilder): Unit = {
    dependencies.foreach(dependency => {
      cpg.dependency
        .nameExact(dependency.getFullDependencyName())
        .where(_.file.nameExact(dependency.filePath))
        .headOption match {
        case Some(dep) =>
          val dependencyRuleId =
            if (ruleCache.checkIfMergedDynamicRuleExist(dependency.ruleId))
              ruleCache.getDynamicMappedInternalRule(dependency.ruleId)
            else dependency.ruleId
          ruleCache.getRuleInfo(dependencyRuleId) match {
            case Some(rule) => Utilities.addRuleTags(builder, dep, rule, ruleCache)
            case None =>
              logger.error(
                s"Dynamic rule ${dependency.ruleId} is not found inside rule cache. It seems given rule is not passed"
              )
          }
        case None =>
      }
    })
  }
}
