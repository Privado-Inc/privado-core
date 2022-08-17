package ai.privado.threatEngine

import ai.privado.exporter.ExporterUtility
import ai.privado.model.{Constants, PolicyOrThreat, PolicyViolationFlowModel}
import ai.privado.utility.Utilities._
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.util.{Failure, Success}

class ThreatEngineExecutor(cpg: Cpg, dataflows: Map[String, Path], repoPath: String) {

  private val logger = LoggerFactory.getLogger(getClass)

  /** Process and return the violating threats of processing type
    * @param threats
    *   threats
    * @return
    */
  def getProcessingViolations(threats: List[PolicyOrThreat]): List[mutable.LinkedHashMap[String, Json]] = {
    threats.flatMap(threat => processProcessingViolations(threat))
  }

  /** Process and return the violating threats of dataflow type
    * @param threats
    *   threats
    * @return
    */
  def getDataflowViolations(threats: List[PolicyOrThreat]): List[mutable.LinkedHashMap[String, Json]] = {
    threats.flatMap(threat => processDataflowViolations(threat))
  }

  private def processProcessingViolations(threat: PolicyOrThreat) = {
    val threatId = threat.id

    // process android threats
    val violationResponse = getAndroidManifestFile(repoPath) match {
      // if we get a manifest file, consider android app
      case Some(manifestFile) =>
        logger.debug(s"Found AndroidManifest.xml: ${manifestFile}")
        logger.info(s"Processing 'processing' threat: ${threatId}")
        threatId match {
          case "Threats.Collection.isKeyboardCacheUsed" =>
            KeyboardCache.getViolations(repoPath) match {
              case Success(res) => Some(res)
              case Failure(e)   => None
            }

          case "Threats.Storage.isAppDataBackupAllowed" =>
            SensitiveDataBackup.getViolations(cpg, manifestFile) match {
              case Success(res) => Some(res)
              case Failure(e)   => None
            }

          case "Threats.Configuration.Mobile.isBackgroundScreenshotEnabled" =>
            BackgroundScreenshot.getViolations(cpg) match {
              case Success(res) => Some(res)
              case Failure(e)   => None
            }

          case "Threats.Sharing.isIpcDataSharingAllowed" =>
            DataSharingIPC.getViolations(cpg, manifestFile) match {
              case Success(res) => Some(res)
              case Failure(e)   => None
            }

          case "Threats.Collection.isInputMasked" =>
            SensitiveInputMask.getViolations(cpg, repoPath) match {
              case Success(res) => Some(res)
              case Failure(e)   => None
            }

          case "Threats.Storage.isDataStoredOnExternalStorage" =>
            DataOnExternalStorage.getViolations(cpg, manifestFile) match {
              case Success(res) => Some(res)
              case Failure(e)   => None
            }

          case _ =>
            logger.debug(s"No implementation detected for threat: ${threatId}")
            None
        }
      case _ =>
        logger.debug("Did not find AndroidManifest.xml")
        None
    }

    violationResponse match {
      case Some((isThreat, occurrences)) if isThreat =>
        val outputMap = mutable.LinkedHashMap[String, Json]()
        outputMap.addOne(Constants.policyId      -> threatId.asJson)
        outputMap.addOne(Constants.policyDetails -> ExporterUtility.getPolicyInfoForExporting(threatId).asJson)
        if (occurrences.nonEmpty) outputMap.addOne(Constants.processing -> occurrences.asJson)
        Some(outputMap)
      case _ => None
    }
  }

  private def processDataflowViolations(threat: PolicyOrThreat) = {
    val threatId = threat.id

    // process android threats
    val violationResponse = getAndroidManifestFile(repoPath) match {
      // if we get a manifest file, consider android app
      case Some(manifestFile) =>
        logger.debug(s"Found AndroidManifest.xml: ${manifestFile}")
        logger.info(s"Processing 'dataflow' threat: ${threatId}")
        threatId match {
          case "Threats.Leakage.isDataLeakingToLog" =>
            DataLeakageToLogs.getViolations(threat, cpg, dataflows) match {
              case Success(res) => Some(res)
              case Failure(e)   => None
            }
          case "Threats.Sharing.isDataExposedToThirdPartiesViaNotification" =>
            DataLeakageToNotifications.getViolations(threat, cpg, dataflows) match {
              case Success(res) => Some(res)
              case Failure(e)   => None
            }

          case _ =>
            logger.debug(s"No implementation detected for threat: ${threatId}")
            None
        }
      case _ =>
        logger.debug("Did not find AndroidManifest.xml")
        None
    }

    violationResponse match {
      case Some((isThreat, dataflows)) if isThreat =>
        val outputMap = mutable.LinkedHashMap[String, Json]()
        outputMap.addOne(Constants.policyId      -> threatId.asJson)
        outputMap.addOne(Constants.policyDetails -> ExporterUtility.getPolicyInfoForExporting(threatId).asJson)
        outputMap.addOne(Constants.dataFlow      -> dataflows.map(flow => convertViolatingFlow(flow)).asJson)
        Some(outputMap)
      case _ => None
    }
  }

  private def convertViolatingFlow(flow: PolicyViolationFlowModel) = {
    val flowOutput = mutable.LinkedHashMap[String, Json]()
    flowOutput.addOne(Constants.sourceId -> flow.sourceId.asJson)
    flowOutput.addOne(Constants.sinkId   -> flow.sinkId.asJson)
    flowOutput.addOne(Constants.pathIds  -> flow.pathIds.asJson)
    flowOutput
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
