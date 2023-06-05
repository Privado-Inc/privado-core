package ai.privado.languageEngine.java.threatEngine

import ai.privado.exporter.ExporterUtility
import ai.privado.languageEngine.java.threatEngine.ThreatUtility.{hasDataElements}
import ai.privado.model.{Constants, PolicyOrThreat}
import ai.privado.model.exporter.ViolationProcessingModel
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.Try

object CustomPrivacyLoggerMustbeUsed {

  private val logger        = LoggerFactory.getLogger(getClass)
  private val MAX_INSTANCES = 5

  /** Check for violation for data leakage to logs threat - consumes already generated dataflows
    *
    * @param cpg
    *   cpg
    * @return
    */
  def getViolations(threat: PolicyOrThreat, cpg: Cpg): Try[(Boolean, List[ViolationProcessingModel])] = Try {
    if (hasDataElements(cpg)) {
      val higherOrderLeakgeSinkId = "Leakages.Log.*"
      val violatingFlows          = ListBuffer[ViolationProcessingModel]()
      val CUSTOM_LOOGER_PATTERN   = getCustomLoggerModulePattern(threat.config)
      val leakageSinks            = cpg.call.where(_.tag.nameExact(Constants.id).value(higherOrderLeakgeSinkId)).l

      leakageSinks
        .distinctBy(_.methodFullName)
        .foreach((leakage) => {
          if (!leakage.methodFullName.matches(CUSTOM_LOOGER_PATTERN)) {
            if (violatingFlows.size < MAX_INSTANCES) {
              violatingFlows
                .append(
                  ViolationProcessingModel(
                    leakage.name,
                    ExporterUtility.convertIndividualPathElement(leakage).get,
                    None
                  )
                )
            }
          }
        })

      (violatingFlows.nonEmpty, violatingFlows.toList)
    } else (false, List())
  }

  private def getCustomLoggerModulePattern(config: Map[String, String]): String = {
    val DEFAULT_PATTERN              = ".*(?:error|severe|fatal|warn|debug|trace|info|log|exception)"
    val CUSTOM_LOGGER_MODULE_PATTERN = "customLoggerModulePattern"
    if (config.contains(CUSTOM_LOGGER_MODULE_PATTERN)) {
      config.getOrElse(CUSTOM_LOGGER_MODULE_PATTERN, DEFAULT_PATTERN)
    } else {
      DEFAULT_PATTERN
    }
  }
}
