package ai.privado.cache

import ai.privado.model.Constants
import org.slf4j.LoggerFactory

import scala.io.Source

object Environment {
  private val logger = LoggerFactory.getLogger(this.getClass)

  val userHash: Option[String]          = sys.env.get("PRIVADO_USER_HASH")
  val dockerAccessKey: Option[String]   = sys.env.get("PRIVADO_DOCKER_ACCESS_KEY")
  val syncToCloud: Option[String]       = sys.env.get("PRIVADO_SYNC_TO_CLOUD")
  val metricsEnabled: Option[String]    = sys.env.get("PRIVADO_METRICS_ENABLED")
  val isProduction: Option[String]      = sys.env.get("IS_PRODUCTION")
  val sessionId: Option[String]         = sys.env.get("PRIVADO_SESSION_ID")
  val hostScanDirectory: Option[String] = sys.env.get("PRIVADO_HOST_SCAN_DIR")
  val privadoVersionCli: Option[String] = sys.env.get("PRIVADO_VERSION_CLI")
  val privadoVersionCore = {
    var version = Constants.notDetected
    try {
      val lines = Source.fromResource("version.txt").getLines()
      for (line <- lines) {
        version = line
      }
    } catch {
      case e: Exception =>
        logger.error("Error fetching version for privado-core")
        logger.debug("Error occurred ", e)
    }
    version
  }
}
