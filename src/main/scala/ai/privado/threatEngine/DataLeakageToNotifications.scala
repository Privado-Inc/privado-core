package ai.privado.threatEngine

import ai.privado.cache.AppCache
import ai.privado.model.{PolicyOrThreat, PolicyViolationFlowModel}
import ai.privado.policyEngine.PolicyExecutor
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.util.Try

object DataLeakageToNotifications {
  private val logger = LoggerFactory.getLogger(getClass)

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    * @param threat
    *   the object of threat rule
    * @param cpg
    *   cpg
    * @param dataflows
    *   generated dataflows for the repo source filepath of manifest file
    * @return
    */
  def getViolations(
    threat: PolicyOrThreat,
    cpg: Cpg,
    dataflows: Map[String, Path]
  ): Try[(Boolean, List[PolicyViolationFlowModel])] = Try {
    // use policy executor to directly process existing flows (we have rule for notifications)
    // we already have this implementation as part of policy enforcement
    // threat being type of suggestive policy
    // might restructure this in future and have central utilities consumed by both
    val policyExecutor = new PolicyExecutor(cpg, dataflows, AppCache.repoName)
    val violatingFlows = policyExecutor.getViolatingFlowsForPolicy(threat)

    // violation if empty
    (violatingFlows.nonEmpty, violatingFlows.toList)
  }
}
