package ai.privado.threatEngine

import ai.privado.exporter.ExporterUtility
import ai.privado.model.{Constants, PolicyOrThreat}
import ai.privado.utility.Utilities._
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.{Failure, Success}

class ThreatEngineExecutor(cpg: Cpg) {

  private val logger     = LoggerFactory.getLogger(getClass)

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
    // process android threats
    val occurrences = getAndroidManifestFile(repoPath) match {
      // if we get a manifest file, consider android app
      case Some(manifestFile) =>
        logger.debug(s"Found AndroidManifest.xml: ${manifestFile}")
        logger.debug(s"Processing threat: ${threatId}")
        threatId match {
          case "Threats.Collection.isKeyboardCacheUsed" =>
            KeyboardCache.getViolations(repoPath) match {
              case Success(occurrences) => Some(occurrences)
              case Failure(e)           => None
            }

          case "Threats.Storage.isAppDataBackupAllowed" =>
            SensitiveDataBackup.getViolations(cpg, manifestFile) match {
              case Success(occurrences) => Some(occurrences)
              case Failure(e)           => None
            }


          case _ =>
            logger.debug(s"No implementation detected for threat: ${threatId}")
            None
        }
      case _ =>
        logger.debug("Did not find AndroidManifest.xml")
        None
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

  private def getAndroidManifestFile(repoPath: String): Option[String] = {
    getAllFilesRecursively(repoPath, Set(".xml")) match {
      case Some(sourceFileNames) => {
        sourceFileNames.find(_.endsWith("AndroidManifest.xml"))
      }
      case None => None
    }
  }
}
