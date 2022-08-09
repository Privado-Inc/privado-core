package ai.privado.metric
import ai.privado.cache.{AppCache, EnvironmentConstant}
import ai.privado.exporter.GitMetaDataExporter
import ai.privado.utility.Utilities
import io.circe.Json
import org.slf4j.LoggerFactory

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import io.circe.parser._
import io.circe.syntax._

object MetricHandler {

  private val logger       = LoggerFactory.getLogger(this.getClass)
  val metricsData          = mutable.HashMap[String, Json]()
  val scanProcessErrors    = ArrayBuffer[String]()
  val totalRulesMatched    = mutable.HashMap[String, Int]()
  val internalRulesMatched = mutable.HashMap[String, Int]()
  val flowCategoryData     = mutable.HashMap[String, Int]()

  metricsData("privadoCoreVersion") = EnvironmentConstant.privadoVersionCore match {
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
    metricsData("internalRuleIdsMatch") = Json.fromValues(internalRulesMatched.keys.map(key => Json.fromString(key)))
    metricsData("scanProcessErrors") = scanProcessErrors.asJson
    metricsData("flowCategoryData") = flowCategoryData.asJson
    metricsData("noOfRulesMatch") = Json.fromInt(totalRulesMatched.size)
    sendDataToServer()
  }

  def sendDataToServer() = {
    // Check if metrics are disabled
    var metricsEndPoint = "https://cli.privado.ai/api/event?version=2"
    EnvironmentConstant.metricsEnabled match {
      case Some(value) =>
        if (value.toBoolean) {
          EnvironmentConstant.privadoDev.getOrElse(0) match {
            case 1 =>
              metricsEndPoint = "https://t.cli.privado.ai/api/event?version=2"
            case _ => ()
          }

          EnvironmentConstant.dockerAccessKey match {
            case Some(dockerKey) =>
              val accessKey = Utilities.getSHA256Hash(dockerKey)
              val requestData = parse(s""" {"event_type": "PRIVADO_CORE",
                                         |  "event_message": ${metricsData.asJson.toString()},
                                         |  "user_hash": "${EnvironmentConstant.userHash.get}",
                                         |  "session_id": "${EnvironmentConstant.sessionId.get}" }""".stripMargin)

              requestData match {
                case Right(data) =>
                  try {
                    requests.post(
                      metricsEndPoint,
                      data = data.toString(),
                      headers = Map("Authentication" -> s"$accessKey", "Content-Type" -> "application/json")
                    )
                  } catch {
                    case e: Exception =>
                      logger.debug("error in uploading metrics to server")
                      logger.debug("The error is ", e)
                  }

                case Left(_) => logger.debug("Error in parsing the metrics data")
              }

            case _ => ()
          }
        }
      case _ => ()
    }
  }
}
