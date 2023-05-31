package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.{hasDataElements}
import ai.privado.model.PolicyOrThreat
import ai.privado.model.exporter.{ViolationProcessingModel}
import ai.privado.policyEngine.PolicyExecutor
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import scala.collection.mutable.ListBuffer
import scala.util.Try

object CustomPrivacyLoggerMustbeUsed {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    *
    * @param cpg
    *   cpg
    * @return
    */
  def getViolations(
    threat: PolicyOrThreat,
    cpg: Cpg,
    dataflows: Map[String, Path],
    ruleCache: RuleCache
  ): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    if (hasDataElements(cpg)) {
      val CUSTOM_LOGGER_ID         = "Leakages.Log.CustomerLogger"
      val violatingFlows           = ListBuffer[ViolationProcessingModel]()
      val policyExecutor           = new PolicyExecutor(cpg, dataflows, AppCache.repoName, ruleCache)
      val leakageSinks             = getSinksMatchingRegex(threat, policyExecutor)
      var isCustomLoggerNotPresent = true

      leakageSinks.foreach(leakageSink => {
        if (leakageSink._1.matches(CUSTOM_LOGGER_ID)) {
          isCustomLoggerNotPresent = false
        } else {
          val otherLogs = policyExecutor.getSourceNode(leakageSink._1).head
          violatingFlows.append(
            ViolationProcessingModel(otherLogs._1, ExporterUtility.convertIndividualPathElement(otherLogs._2).get)
          )
        }
      })

      (isCustomLoggerNotPresent, violatingFlows.toList)
    } else (false, List())
  }

  private def getSinksMatchingRegex(threat: PolicyOrThreat, policyExecutor: PolicyExecutor) = {
    policyExecutor.dataflowSinkIdMap.flatMap(sinkIdEntry => {
      if (sinkIdEntry._1.matches("Leakages.*"))
        Some(sinkIdEntry._1, sinkIdEntry._2)
      else
        None
    })
  }
}
