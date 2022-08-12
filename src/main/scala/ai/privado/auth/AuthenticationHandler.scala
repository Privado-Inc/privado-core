package ai.privado.auth
import ai.privado.cache.Environment
import ai.privado.metric.MetricHandler
import ai.privado.utility.Utilities
import io.circe.Json
import org.slf4j.LoggerFactory

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Paths}
import java.security.{DigestInputStream, MessageDigest}

object AuthenticationHandler {
  /*
   * To handle the cloud flow for scanned repositories. Assumes the flag for auth is enabled.
   * Asks for consent from the user and then decides the flow for Privado Cloud APIs.
   */
  private val logger = LoggerFactory.getLogger(this.getClass)

  def syncToCloud: Boolean = {
    try {
      Environment.syncToCloud.getOrElse("False").toBoolean
    } catch {
      case _: Exception => false
    }
  }

  def authenticate(repoPath: String): Unit = {
    Environment.dockerAccessKey match {
      case Some(_) =>
        Environment.userHash match {
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

  // Compute a hash of a file
  // The output of this function should match the output of running "md5 -q <file>"
  def computeHash(path: String): String = {
    val buffer = new Array[Byte](8192)
    val md5    = MessageDigest.getInstance("MD5")

    val dis = new DigestInputStream(new FileInputStream(new File(path)), md5)
    try { while (dis.read(buffer) != -1) {} }
    finally { dis.close() }

    md5.digest.map("%02x".format(_)).mkString
  }

  def pushDataToCloud(repoPath: String): String = {
    var BASE_URL = "https://api.code.privado.ai/prod"
    if (!Environment.isProduction.getOrElse("False").toBoolean) {
      BASE_URL = "https://t.api.code.privado.ai/test"
    }
    try {
      val file                         = new File(s"$repoPath/.privado/privado.json")
      val md5hash                      = computeHash(s"$repoPath/.privado/privado.json")
      val accessKey: String            = Utilities.getSHA256Hash(Environment.dockerAccessKey.get)
      val s3PresignGenEndpoint: String = s"$BASE_URL/cli/api/file/presigned/${Environment.userHash.get}/${md5hash}"
      val firstResp = requests.get(url = s3PresignGenEndpoint, headers = Map("Authentication" -> s"$accessKey"))
      firstResp.statusCode match {
        case 200 =>
          val fresp = ujson.read(firstResp.text())
          val secResp = requests.post(
            url = fresp("s3PreSignedUrl")("url").str,
            data = requests.MultiPart(
              requests.MultiItem("key", fresp("s3PreSignedUrl")("fields")("key").str),
              requests.MultiItem("x-amz-algorithm", fresp("s3PreSignedUrl")("fields")("x-amz-algorithm").str),
              requests.MultiItem("x-amz-credential", fresp("s3PreSignedUrl")("fields")("x-amz-credential").str),
              requests.MultiItem("x-amz-date", fresp("s3PreSignedUrl")("fields")("x-amz-date").str),
              requests.MultiItem("policy", fresp("s3PreSignedUrl")("fields")("policy").str),
              requests.MultiItem("x-amz-signature", fresp("s3PreSignedUrl")("fields")("x-amz-signature").str),
              requests.MultiItem("file", file)
            )
          )
          secResp.statusCode match {
            case 204 =>
              val finalResp = requests.post(
                url =
                  s"$BASE_URL/cli/api/file/process/${Environment.userHash.get}?filePath=${fresp("s3PreSignedUrl")("fields")("key").str}",
                headers = Map("Authentication" -> s"$accessKey")
              )
              val json = ujson.read(finalResp.text())
              finalResp.statusCode match {
                case 200 =>
                  s"""\n> Successfully synchronized results with Privado Cloud \n> Continue to view results on: ${json(
                      "redirectUrl"
                    ).toString()}\n"""
                case _ =>
                  logger.debug("Error while upload file to server")
                  "Error occurred while uploading the file to the cloud."
              }
            case _ =>
              logger.debug("Error while upload file to server")
              "Error occurred while uploading the file to the cloud."
          }

        case _ =>
          logger.debug("Error while generating upload request")
          "Error occurred while uploading the file to the cloud."
      }

    } catch {
      case e: Exception =>
        logger.error("Error occurred while uploading the file to the cloud.")
        logger.debug("Error:", e)
        s"Error Occurred. ${e.toString}"
    }
  }

  def doesResultFileExist(repoPath: String): Boolean = {
    Files.exists(Paths.get(repoPath, ".privado", "privado.json"))
  }

}
