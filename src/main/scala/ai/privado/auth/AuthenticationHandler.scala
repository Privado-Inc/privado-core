package ai.privado.auth
import java.security.MessageDigest
import java.math.BigInteger
import java.nio.file.{Path, Paths}

object AuthenticationHandler {
  /*
   * To handle the cloud flow for scanned repositories. Assumes the flag for auth is enabled.
   * Asks for consent from the user and then decides the flow for Privado Cloud APIs.
   */

  val userHash: String        = sys.env.getOrElse("PRIVADO_USER_HASH", null)
  val syncToCloud: Boolean    = sys.env.getOrElse("PRIVADO_SYNC_TO_CLOUD", "False").toBoolean
  val dockerAccessKey: String = sys.env.getOrElse("PRIVADO_DOCKER_ACCESS_KEY", null)

  def authenticate(repoPath: String): Unit = {
    dockerAccessKey match {
      case null => () // No auth flow happens if docker access key is not present
      case _ =>
        var syncPermission: Boolean = true
        if (!syncToCloud) {
          syncPermission = askForPermission() // Ask user for request permissions
        }
        if (syncPermission) {
          println(pushDataToCloud(repoPath))
        } else {
          ()
        }
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

  def updateConfigFile(property: String, value: String): Unit = {
    val jsonString = os.read(os.Path("/app/config/config.json"))
    val data       = ujson.read(jsonString)
    data(property) = value
    os.write.over(os.Path("/app/config/config.json"), data)
  }

  def pushDataToCloud(repoPath: String): String = {
    // TODO change BASE_URL and upload url for prod
    val BASE_URL = "https://t.api.code.privado.ai/test"
    val file = os.Path {
      s"$repoPath/.privado/privado.json"
    }
    val uploadURL: String = s"$BASE_URL/cli/api/$userHash/file"

    val accessKey: String = {
      String.format(
        "%032x",
        new BigInteger(1, MessageDigest.getInstance("SHA-256").digest(dockerAccessKey.getBytes("UTF-8")))
      )
    }

    val response = requests.post(
      uploadURL,
      data = requests.MultiPart(requests.MultiItem("File2", data = Paths.get(file.toString()))),
      headers = Map("access-key" -> s"$accessKey")
    )

    // TODO confirm status code from team. Using 201 as a placeholder
    response.statusCode match {
      case 201 => response.data.toString()
      case _   => s"Error Occurred. ${response.data.toString()}"
    }
  }
}
