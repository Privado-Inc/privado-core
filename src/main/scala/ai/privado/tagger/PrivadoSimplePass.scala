package ai.privado.tagger

import ai.privado.cache.RuleCache
import ai.privado.metric.MetricHandler
import ai.privado.model.RuleInfo
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory

abstract class PrivadoSimplePass(cpg: Cpg) extends SimpleCpgPass(cpg) {

  var ruleInfo: RuleInfo = null
  val logger             = LoggerFactory.getLogger(getClass)

  /** Helper function to set the rule and apply the pass
    */
  def setRuleAndApply(ruleInfo: RuleInfo) = {
    try {
      this.ruleInfo = ruleInfo
      this.createAndApply()
      MetricHandler.totalRulesMatched += 1
      RuleCache.internalRules.get(ruleInfo.id) match {
        case Some(value) => RuleCache.internalRules(ruleInfo.id) += 1
        case (_)         => ()
      }
    } catch {
      case ex: Exception => {
        logger.error("Exception executing pass")
        MetricHandler.scanProcessErrors.addOne(ex.toString)
      }
    }
  }

}
