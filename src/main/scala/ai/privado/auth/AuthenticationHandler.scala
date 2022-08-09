package ai.privado.auth
import ai.privado.cache.EnvironmentConstant
import ai.privado.metric.MetricHandler
import ai.privado.utility.Utilities
import io.circe.Json
import org.slf4j.LoggerFactory

import java.io.File

object AuthenticationHandler {
  /*
   * To handle the cloud flow for scanned repositories. Assumes the flag for auth is enabled.
   * Asks for consent from the user and then decides the flow for Privado Cloud APIs.
   */
  private val logger = LoggerFactory.getLogger(this.getClass)

  def syncToCloud: Boolean = {
    try {
      EnvironmentConstant.syncToCloud.getOrElse("False").toBoolean
    } catch {
      case _: Exception => false
    }
  }

  def authenticate(repoPath: String): Unit = {
    EnvironmentConstant.dockerAccessKey match {
      case Some(_) =>
        EnvironmentConstant.userHash match {
          case Some(_) =>
            var syncPermission: Boolean = true
            if (!syncToCloud) {
              syncPermission = askForPermission() // Ask user for request permissions
            }
            if (syncPermission) {
              println(MetricHandler.timeMetric(pushDataToCloud(repoPath), "UploadFile"))
            } else {
              ()
            }
          case _ => ()
        }
      case _ => ()
    }
  }

  def askForPermission(): Boolean = {
    println("Do you want to visualize these results on our Privacy View Cloud Dashboard? (Y/n)")
    val userPermissionInput = scala.io.StdIn.readLine().toLowerCase
    var cloudConsentPermission: Boolean = userPermissionInput match {
      case "n" | "no" | "0" => false
      case _ =>
        updateConfigFile("syncToPrivadoCloud", "true")
        true
    }
    MetricHandler.metricsData("cloudConsentEvent") = Json.fromBoolean(cloudConsentPermission)
    cloudConsentPermission
  }

  def updateConfigFile(property: String, value: String): Boolean = {
    try {
      val jsonString = os.read(os.Path("/app/config/config.json"))
      val data       = ujson.read(jsonString)

      try {
        data(property) = ujson.Value(value)
      } catch {
        case _: ujson.ParseException =>
          data(property) = ujson.Str(value)
      }
      val outputStream = os.write.over.outputStream(os.Path("/app/config/config.json"))
      ujson.writeToOutputStream(data, outputStream, indent = 4)
      true
    } catch {
      case e: Exception =>
        logger.error(s"Error while updating the config file")
        logger.debug(s"File update error: ", e)
        false
    }
  }

  def pushDataToCloud(repoPath: String): String = {
    var BASE_URL = "https://api.code.privado.ai/prod/"
    EnvironmentConstant.isProduction.getOrElse(0) match {
      case 0 => BASE_URL = "https://t.api.code.privado.ai/test"
      case _ => ()
    }
    val file              = new File(s"$repoPath/.privado/privado.json")
    val uploadURL: String = s"$BASE_URL/cli/api/file/${EnvironmentConstant.userHash.get}"
    val accessKey: String = Utilities.getSHA256Hash(EnvironmentConstant.dockerAccessKey.get)
    try {
      val response = requests.post(
        uploadURL,
        data = requests.MultiPart(requests.MultiItem("scanfile", file, file.getName)),
        headers = Map("Authentication" -> s"$accessKey")
      )
      val json = ujson.read(response.text())
      response.statusCode match {
        case 200 =>
          s"""\n> Successfully synchronized results with Privado Cloud \n> Continue to view results on: ${json(
              "redirectUrl"
            ).toString()}\n"""
        case _ => json("message").toString()
      }

    } catch {
      case e: Exception =>
        logger.error("Error occurred while uploading the file to the cloud.")
        logger.debug("Error:", e)
        s"Error Occurred. ${e.toString}"
    }
  }
}
