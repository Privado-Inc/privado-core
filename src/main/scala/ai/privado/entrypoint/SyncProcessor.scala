package ai.privado.entrypoint

import ai.privado.auth.AuthenticationHandler
import ai.privado.cache.Environment
import ai.privado.model.Constants
import ai.privado.model.Constants.{outputDirectoryName, outputFileName}
import better.files.File
import org.slf4j.LoggerFactory

object SyncProcessor extends CommandProcessor {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def checkOutputFileExists(): Boolean = {
    val repoPath = config.sourceLocation.head
    File(s"$repoPath/$outputDirectoryName/$outputFileName.json").exists
  }

  override def process(): Either[String, Unit] = {
    println(s"Privado CLI Version: ${Environment.privadoVersionCli.getOrElse(Constants.notDetected)}")
    println(s"Privado Core Version: ${Environment.privadoVersionCore}")
    checkOutputFileExists() match {
      case true =>
        Right(AuthenticationHandler.authenticate(config.sourceLocation.head))
      case false =>
        logger.debug("Output file not found")
        println("Output file does not exist. Please Scan the repository to upload the file!")
        Left("Output file does not exist.")
    }
  }
  override var config: PrivadoInput = _
}
