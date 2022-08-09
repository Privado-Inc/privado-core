package ai.privado.cache

object EnvironmentConstant {
  val privadoVersionCore: Option[String] = sys.env.get("PRIVADO_VERSION_CORE")
  val userHash: Option[String]           = sys.env.get("PRIVADO_USER_HASH")
  val dockerAccessKey: Option[String]    = sys.env.get("PRIVADO_DOCKER_ACCESS_KEY")
  val syncToCloud: Option[String]        = sys.env.get("PRIVADO_SYNC_TO_CLOUD")
  val metricsEnabled: Option[String]     = sys.env.get("PRIVADO_METRICS_ENABLED")
  val privadoDev: Option[String]         = sys.env.get("PRIVADO_DEV")
  val sessionId: Option[String]          = sys.env.get("PRIVADO_SESSION_ID")
  val hostScanDirectory: Option[String]  = sys.env.get("PRIVADO_HOST_SCAN_DIR")
}
