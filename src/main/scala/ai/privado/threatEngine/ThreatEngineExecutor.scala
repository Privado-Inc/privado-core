package ai.privado.threatEngine

import ai.privado.exporter.ExporterUtility
import ai.privado.model.{Constants, PolicyOrThreat}
import io.circe.Json
import io.circe.syntax.EncoderOps

import scala.collection.mutable
import scala.util.{Failure, Success}

class ThreatEngineExecutor {

  /** Process and return the violating threats
    * @param threats
    * @param repoPath
    * @return
    */
  def execute(threats: List[PolicyOrThreat], repoPath: String) = {
    threats.flatMap(threat => process(threat, repoPath))
  }

  private def process(threat: PolicyOrThreat, repoPath: String) = {
    val threatId = threat.id
    val occurrences = threatId match {
      case "Threats.Collection.isKeyboardCacheUsed" =>
        KeyboardCache.getViolations(repoPath) match {
          case Success(occurrences) => Some(occurrences)
          case Failure(_)           => None
        }
      case _ => None
    }

    occurrences match {
      case Some(occurrences) if (occurrences.nonEmpty) =>
        val outputMap = mutable.LinkedHashMap[String, Json]()
        outputMap.addOne(Constants.policyId      -> threatId.asJson)
        outputMap.addOne(Constants.policyDetails -> ExporterUtility.getPolicyInfoForExporting(threatId).asJson)
        outputMap.addOne(Constants.processing    -> occurrences.asJson)
        Some(outputMap)
      case _ => None
    }

  }
}
