package ai.privado.entrypoint

import ai.privado.cache.AppCache
import ai.privado.metadata.SystemInfo
import ai.privado.model.Constants

import scala.util.{Failure, Success, Try}

object MetadataProcessor extends CommandProcessor {

  override def process(appCache: AppCache): Either[String, Unit] = {

    def generateMetadata(): SystemInfo = {
      val systemInfo = SystemInfo.getInfo
      SystemInfo.dumpInfoToFile(config.sourceLocation.head, Constants.systemInfoFileName, systemInfo)
      systemInfo
    }

    Try(generateMetadata()) match
      case Failure(exception) =>
        println(s"Exception when processing metadata command : ${exception.toString}")
        Left(exception.toString)
      case Success(systemInfo) => Right(systemInfo)
  }

}
