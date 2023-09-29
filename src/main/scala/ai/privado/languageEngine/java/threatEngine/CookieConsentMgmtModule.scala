package ai.privado.languageEngine.java.threatEngine

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.hasDataElements
import ai.privado.model.PolicyOrThreat
import ai.privado.model.exporter.{ViolationProcessingModel}
import ai.privado.policyEngine.PolicyExecutor
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.util.Try

object CookieConsentMgmtModule {

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
      val policyExecutor = new PolicyExecutor(cpg, dataflows, AppCache.repoName, ruleCache)
      val violatingFlows = policyExecutor.getViolatingOccurrencesForPolicy(threat)

      val consentMgmtModulePresent      = cpg.call.methodFullName(getCookieConsentMgmtModulePattern(threat.config))
      val prebidNonStandardIntergration = cpg.call("__tcfapi")
      val prebidStandardIntegration     = cpg.call.methodFullName(".*pbjs.*setConfig")

      // violation if empty
      (
        consentMgmtModulePresent.isEmpty && prebidStandardIntegration.isEmpty && prebidNonStandardIntergration.isEmpty,
        violatingFlows.toList.distinctBy(_.sourceId)
      )
    } else (false, List())
  }

  private def getCookieConsentMgmtModulePattern(config: Map[String, String]): String = {
    val DEFAULT_PATTERN                    = "(ngx-cookieconsent).*"
    val COOKIE_CONSENT_MGMT_MODULE_PATTERN = "cookieConsentMgmtModulePattern"
    if (config.contains(COOKIE_CONSENT_MGMT_MODULE_PATTERN)) {
      config.get(COOKIE_CONSENT_MGMT_MODULE_PATTERN).getOrElse(DEFAULT_PATTERN)
    } else {
      DEFAULT_PATTERN
    }
  }
}
