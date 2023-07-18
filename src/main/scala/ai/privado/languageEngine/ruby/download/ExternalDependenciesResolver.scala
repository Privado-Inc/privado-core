package ai.privado.languageEngine.ruby.download

import ai.privado.languageEngine.ruby.passes.download.ExternalDependenciesPass
import better.files.File
import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.utils.PackageTable
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import org.slf4j.LoggerFactory

import sys.process.*
import scala.util.{Failure, Success}

object ExternalDependenciesResolver {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private def downloadGemDependency(tempPath: String, gemName: String, gemVersion: String): Unit = {
    val gemCommand = s"gem fetch $gemName ${if (gemVersion.nonEmpty) s"-v $gemVersion" else ""}"
    ExternalCommand.run(gemCommand, tempPath) match
      case Success(output) =>
        logger.info(s"Gem successfully downloaded: $tempPath")
      case Failure(exception) =>
        logger.error(s"Error while downloading dependency: ${exception.getMessage}")
  }

  private def unpackGemDependency(tempPath: String): Unit = {
    val currentDir  = File(tempPath)
    val gemFileList = currentDir.list.filter(_.extension.exists(_ == ".gem")).map(_.path.toString).toList
    gemFileList.foreach(gemFile => {
      ExternalCommand.run(s"gem unpack $gemFile", s"${tempPath.toString}/unpack") match
        case Success(output) =>
          logger.info(s"Gem unpacked Successfully: $output")
        case Failure(exception) =>
          logger.error(s"Error while unpacking: ${exception.getMessage}")
    })
  }

  private def fetchDependencyList(inputPath: String): List[(String, String)] = {
    val gemFileContent = File(s"$inputPath${java.io.File.separator}Gemfile").contentAsString
    val gemRegex       = """gem ['"]([^'"]+)['"](?:,\s*['"]([^'"]+)['"])?""".r

    gemRegex
      .findAllMatchIn(gemFileContent)
      .flatMap { matchResult =>
        val gemName    = matchResult.group(1)
        val gemVersion = Option(matchResult.group(2)).map(extractVersion => extractVersion).getOrElse("")
        Some(gemName -> gemVersion)
      }
      .toList
      .distinctBy(_._1)
  }

  private def extractVersion(version: String): String = {
    val versionRegex = """(?:[>=<~]+)\s*([\d.]+)""".r
    version match {
      case versionRegex(versionNumber) => versionNumber
      case _                           => ""
    }
  }

  def downloadDependencies(cpg: Cpg, inputPath: String): PackageTable = {
    val packageTable = new PackageTable()
    if (File(s"${inputPath}${java.io.File.separator}Gemfile").exists) {
      val dependenciesList = fetchDependencyList(inputPath)
      if (checkDownloadPrerequisite()) {
        val tempDir = File.newTemporaryDirectory()
        (tempDir / "unpack").createDirectoryIfNotExists()
        try {
          dependenciesList.foreach((gemName, version) => {
            downloadGemDependency(tempDir.toString, gemName, extractVersion(version))
          })
          unpackGemDependency(tempDir.toString)
          new ExternalDependenciesPass(cpg, tempDir.toString(), packageTable, inputPath).createAndApply()
        } catch {
          case ex: Exception =>
            println(s"Error while parsing dependency: ${ex.getMessage}")
        } finally {
          tempDir.delete()
        }
      }
    }
    packageTable
  }

  private def checkDownloadPrerequisite(): Boolean = {
    val isRubyInstalled = "ruby -v".! == 0
    if (!isRubyInstalled) {
      logger.error("Skipping Dependency Download: Ruby is not installed.")
    }
    isRubyInstalled
  }
}
