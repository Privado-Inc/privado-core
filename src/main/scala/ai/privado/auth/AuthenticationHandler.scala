package ai.privado.auth
import org.slf4j.LoggerFactory

import java.math.BigInteger
import java.io.File
import java.security.MessageDigest

object AuthenticationHandler {
  /*
   * To handle the cloud flow for scanned repositories. Assumes the flag for auth is enabled.
   * Asks for consent from the user and then decides the flow for Privado Cloud APIs.
   */
  private val logger                  = LoggerFactory.getLogger(this.getClass)
  val userHash: Option[String]        = sys.env.get("PRIVADO_USER_HASH")
  val dockerAccessKey: Option[String] = sys.env.get("PRIVADO_DOCKER_ACCESS_KEY")
  def syncToCloud: Boolean = {
    try {
      sys.env.getOrElse("PRIVADO_SYNC_TO_CLOUD", "False").toBoolean
    } catch {
      case _: Exception => false
    }
  }

  def authenticate(repoPath: String): Unit = {
    dockerAccessKey match {
      case Some(_) =>
        var syncPermission: Boolean = true
        if (!syncToCloud) {
          syncPermission = askForPermission() // Ask user for request permissions
        }
        if (syncPermission) {
          println(pushDataToCloud(repoPath))
        } else {
          ()
        }
      case _ => ()
    }
  }

  def askForPermission(): Boolean = {
    println("Do you want to visualize these results on our Privacy View Cloud Dashboard? (Y/n)")
    val userPermissionInput = scala.io.StdIn.readLine().toLowerCase
    userPermissionInput match {
      case "n" | "no" | "0" => false
      case _ =>
        updateConfigFile("syncToPrivadoCloud", "true")
        true
    }
  }

  def updateConfigFile(property: String, value: String): Boolean = {
    try {
      val jsonString = os.read(os.Path("/app/config/config.json"))
      val data       = ujson.read(jsonString)
      data(property) = value
      os.write.over(os.Path("/app/config/config.json"), data)
      true
    } catch {
      case e: Exception =>
        logger.error(s"Error while updating the config file")
        logger.debug(s"File update error: ", e)
        false
    }
  }

  def pushDataToCloud(repoPath: String): String = {
    // TODO change BASE_URL and upload url for prod
    val BASE_URL          = "https://t.api.code.privado.ai/test"
    val file              = new File(s"$repoPath/.privado/privado.json")
    val uploadURL: String = s"$BASE_URL/cli/api/file/$userHash"

    val accessKey: String = {
      String.format(
        "%032x",
        new BigInteger(1, MessageDigest.getInstance("SHA-256").digest(dockerAccessKey.get.getBytes("UTF-8")))
      )
    }

    try {
      val response = requests.post(
        uploadURL,
        data = requests.MultiPart(requests.MultiItem("scanfile", file, file.getName)),
        headers = Map("access-key" -> s"$accessKey")
      )
      val json = ujson.read(response.text())
      response.statusCode match {
        case 200 => s"""\n> Successfully synchronized results with Privado Cloud \n
          > Continue to view results on: ${json("redirectUrl").toString()}\n"""
        case _ => json("message").toString()
      }

    } catch {
      case e: Exception => {
        logger.error("Error occurred while uploading the file to the cloud.")
        logger.debug("Error:", e)
        s"Error Occurred. ${e.toString}"
      }
    }
  }
}
