package ai.privado.metric
import ai.privado.cache.{AppCache, Environment}
import ai.privado.exporter.GitMetaDataExporter
import ai.privado.utility.Utilities
import io.circe.Json
import org.slf4j.LoggerFactory

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import io.circe.syntax._

object MetricHandler {

  private val logger       = LoggerFactory.getLogger(this.getClass)
  val metricsData          = mutable.HashMap[String, Json]()
  val scanProcessErrors    = ArrayBuffer[String]()
  val totalRulesMatched    = mutable.Set[String]()
  val internalRulesMatched = mutable.Set[String]()
  val flowCategoryData     = mutable.HashMap[String, Int]()

  metricsData("privadoCoreVersion") = Environment.privadoVersionCore match {
    case Some(value) => Json.fromString(value)
    case _           => Json.Null
  }
  metricsData("privadoCoreCommand") = Json.Null
  val gitMetaData = GitMetaDataExporter.getMetaData(AppCache.localScanPath)
  metricsData("hashedRepoIdentifier") = Json.fromString(Utilities.getSHA256Hash(gitMetaData.size match {
    case 0 => AppCache.repoName
    case _ => gitMetaData("remoteUrl")
  }))

  def timeMetric[R](block: => R, call: String): R = {
    val startTime = System.nanoTime()
    val result    = block
    val endTime   = System.nanoTime()
    metricsData(s"timeTakenTo$call") =
      Json.fromLong(TimeUnit.SECONDS.convert((endTime - startTime), TimeUnit.NANOSECONDS))
    result
  }

  def compileAndSend() = {
    metricsData("internalRuleIdsMatch") = Json.fromValues(internalRulesMatched.map(key => Json.fromString(key)))
    metricsData("scanProcessErrors") = scanProcessErrors.asJson
    metricsData("flowCategoryData") = flowCategoryData.asJson
    metricsData("noOfRulesMatch") = Json.fromInt(totalRulesMatched.size)
    sendDataToServer()
  }

  def sendDataToServer() = {
    // Check if metrics are disabled
    var metricsEndPoint = "https://cli.privado.ai/api/event?version=2"
    Environment.metricsEnabled match {
      case Some(value) =>
        if (value.toBoolean) {
          if (!Environment.isProduction.getOrElse("False").toBoolean) {
                metricsEndPoint = "https://t.cli.privado.ai/api/event?version=2"
          }
          Environment.dockerAccessKey match {
            case Some(dockerKey) =>
              val accessKey = Utilities.getSHA256Hash(dockerKey)
              val requestData = s""" {"event_type": "PRIVADO_CORE",
                                         |  "event_message": ${metricsData.asJson.spaces4},
                                         |  "user_hash": "${Environment.userHash.get}",
                                         |  "session_id": "${Environment.sessionId.get}" }""".stripMargin
              try {
                requests.post(
                  metricsEndPoint,
                  data = requestData,
                  headers = Map("Authentication" -> s"$accessKey", "Content-Type" -> "application/json")
                )
              } catch {
                case e: Exception =>
                  logger.debug("error in uploading metrics to server")
                  logger.debug("The error is ", e)
              }
            case _ => ()
          }
        }
      case _ => ()
    }
  }
}
