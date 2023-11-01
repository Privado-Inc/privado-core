package ai.privado.languageEngine.ruby.passes.download

import ai.privado.cache.RuleCache
import ai.privado.utility.ConcurrentProcessor
import better.files.File
import io.joern.rubysrc2cpg.deprecated.utils.PackageTable
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import org.jruby.ast.*
import org.jruby.{Ruby, RubyInstanceConfig}
import org.slf4j.LoggerFactory

import java.io.FileInputStream
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.sys.process.*
import scala.util.{Failure, Success}

object JRubyBasedParser {
  private final val jruby = Ruby.newInstance(new RubyInstanceConfig(true))

  def parseFile(filename: String): Node =
    jruby.parseFile(new FileInputStream(filename), filename, jruby.getCurrentContext.getCurrentScope)

  def parseString(code: String, filename: String): Node = {
    jruby.parse(code, filename, jruby.getCurrentContext.getCurrentScope, 1, true)
  }
}

case class TypeDeclMetaData(gemOrFileName: String, typeDeclName: String, typeDeclPath: String)
case class ModuleMetaData(gemOrFileName: String, moduleName: String, modulePath: String)
case class PackageMethodMetaData(moduleName: String, methodName: String, parentClassPath: String, classType: String)

class DownloadDependenciesPass(packageTable: PackageTable, inputPath: String, rules: RuleCache)
    extends ConcurrentProcessor[String, PackageTable](packageTable, rules) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  private val GEMFILE      = "Gemfile"
  private val GEMFILE_LOCK = "Gemfile.lock"
  private val GEMSPEC      = "gemspec"

  val tempLocation = {
    val tempDir = File.newTemporaryDirectory()
    (tempDir / "unpack").createDirectoryIfNotExists()
    tempDir
  }
  override def generateParts(): Array[String] = {
    val gemFiles = File(inputPath).listRecursively
      .filter(file => file.isRegularFile && file.name.matches("Gemfile|Gemfile[.]lock|.*[.]gemspec"))
      .map(file =>
        file.name match
          case GEMFILE      => (file, GEMFILE)
          case GEMFILE_LOCK => (file, GEMFILE_LOCK)
          case _            => (file, GEMSPEC)
      )
      .toList
    if (checkDownloadPrerequisite() && gemFiles.nonEmpty) {
      gemFiles.foreach { gemFileWithType =>
        val (gemFile, gemType) = gemFileWithType
        processGem(gemFile, gemType, tempLocation.toString)
      }
      tempLocation.listRecursively.filter(_.extension.exists(_ == ".rb")).map(_.path.toString).toArray
    } else {
      Array.empty
    }
  }

  // TODO: Add unit test for this method
  def processGem(gemFile: File, gemType: String, downloadAndUnpackLocation: String): Unit = {
    val dependenciesList = getDependencyList(gemFile, gemType)
    val futureTasks = dependenciesList.map((gemName, version) => {
      Future {
        downloadAndUnpackDependency(downloadAndUnpackLocation, gemName, extractGemVersion(version))
      }
    })
    val allResults: Future[List[Unit]] = Future.sequence(futureTasks)
    Await.result(allResults, Duration.Inf)
  }

  def downloadAndUnpackDependency(downloadAndUnpackLocation: String, gemName: String, gemVersion: String): Unit = {
    val gemCommand = s"gem fetch $gemName ${if (gemVersion.nonEmpty) s"-v $gemVersion" else ""}"
    ExternalCommand.run(gemCommand, downloadAndUnpackLocation) match
      case Success(output) =>
        output.collectFirst { case line: String if line.startsWith("Downloaded") => line.split("\\s+").last } match
          case Some(dotGem) =>
            val dogGemFile = s"${downloadAndUnpackLocation}${java.io.File.separator}${dotGem}.gem"
            logger.info(s"Gem successfully downloaded: '$dogGemFile'")
            ExternalCommand.run(
              s"gem unpack $dogGemFile",
              s"${downloadAndUnpackLocation}${java.io.File.separator}unpack"
            ) match
              case Success(output) =>
                // Check if child Gemfile exists and requires further download
                val internalGemFile = File(
                  s"${downloadAndUnpackLocation}${java.io.File.separator}unpack${java.io.File.separator}dotGem${java.io.File.separator}Gemfile"
                )
                if (internalGemFile.exists) then processGem(internalGemFile, GEMFILE, downloadAndUnpackLocation)
                logger.info(s"Gem unpacked Successfully: '$dogGemFile'")
              case Failure(exception) =>
                logger.warn(s"Error while unpacking '$dogGemFile' : ", exception)
          case _ => println(s"Something is not working..... $output") // TODO: Removed this line once testing is done.

      case Failure(exception) =>
        logger.warn(s"Error while downloading dependency '${gemName}-${gemVersion}': ", exception)
  }

  // TODO: Add unit test for this method
  def getDependencyList(gemFile: File, gemType: String): List[(String, String)] = {

    val gemFileContent = gemFile.lines.map(_.trim).toList
    gemType match {
      case GEMFILE =>
        val gemRegex = """gem ['"]([^'"]+)['"](?:,\s*['"]([^'"]+)['"])?"""
        gemFileContent.filter(_.matches(gemRegex)).flatMap { gemLine =>
          gemRegex.r
            .findAllMatchIn(gemLine)
            .flatMap { matchResult =>
              val gemName    = matchResult.group(1)
              val gemVersion = Option(matchResult.group(2)).map(extractVersion => extractVersion).getOrElse("")
              Some(gemName -> gemVersion)
            }
            .toList
            .distinctBy(_._1)
        }
      case GEMFILE_LOCK =>
        val gemRegex = """([\w-]+) (?:[(](.+)[)])?"""
        gemFileContent.filter(_.matches(gemRegex)).flatMap { gemLine =>
          gemRegex.r
            .findAllMatchIn(gemLine)
            .flatMap { matchResult =>
              val gemName    = matchResult.group(1)
              val gemVersion = Option(matchResult.group(2)).map(extractVersion => extractVersion).getOrElse("")
              Some(gemName -> gemVersion)
            }
            .toList
            .distinctBy(_._1)
        }
      case GEMSPEC =>
        val gemRegex = """spec[.]add(?:_|_runtime_|_development_)dependency ['"]([^'"]+)['"](?:,\s*['"]([^'"]+)['"])?"""
        gemFileContent.filter(_.matches(gemRegex)).flatMap { gemLine =>
          gemRegex.r
            .findAllMatchIn(gemLine)
            .flatMap { matchResult =>
              val gemName    = matchResult.group(1)
              val gemVersion = Option(matchResult.group(2)).map(extractVersion => extractVersion).getOrElse("")
              Some(gemName -> gemVersion)
            }
            .toList
            .distinctBy(_._1)
        }
    }
  }

  // TODO: Add unit test for this method
  def extractGemVersion(version: String): String = {
    val versionRegex = """(?:[>=<~]+)\s*([\d.]+)""".r
    version match {
      case versionRegex(versionNumber) => versionNumber
      case _                           => ""
    }
  }

  def checkDownloadPrerequisite(): Boolean = {
    val isRubyInstalled = "ruby -v".! == 0
    if (!isRubyInstalled) {
      logger.error("Skipping Dependency Download: Ruby is not installed.")
    }
    isRubyInstalled
  }
  override def runOnPart(filePath: String): Unit = {
    val moduleName = resolveModuleNameFromPath(filePath)
    try {
      val rootNode = JRubyBasedParser.parseFile(filePath)
      fetchMethodInfoFromNode(filePath, rootNode, List.empty, moduleName)
    } catch {
      case ex: Exception =>
        logger.error(s"Error while parsing dependency file ${filePath} : ", ex)
    }
  }

  def fetchMethodInfoFromNode(
    filePath: String,
    node: Node,
    currentNameSpace: List[String],
    moduleName: String
  ): Unit = {
    try {
      if (node != null) {
        node.getNodeType match {
          case NodeType.CLASSNODE | NodeType.MODULENODE =>
            val childList = node.childNodes().asScala.toList
            if (childList.nonEmpty) {
              val classOrModuleName = childList.head.asInstanceOf[Colon2Node].getName.toString
              val classPath         = currentNameSpace.mkString(".")
              if (node.getNodeType == NodeType.CLASSNODE)
                addInWriterQueue(
                  "ADD_TYPE_DECL",
                  TypeDeclMetaData(moduleName, classOrModuleName, s"$classPath.$classOrModuleName".stripPrefix("."))
                )
              else
                addInWriterQueue(
                  "ADD_MODULE",
                  ModuleMetaData(moduleName, classOrModuleName, s"$classPath.$classOrModuleName".stripPrefix("."))
                )
              node
                .childNodes()
                .forEach(childNode =>
                  fetchMethodInfoFromNode(filePath, childNode, currentNameSpace ++ Seq(classOrModuleName), moduleName)
                )
            }
          case NodeType.DEFNNODE =>
            val methodName = node.asInstanceOf[DefnNode].getName.toString
            val classPath  = currentNameSpace.mkString(".")
            addInWriterQueue(
              "ADD_PACKAGE_METHOD",
              PackageMethodMetaData(moduleName, methodName, s"$classPath.", "<extMod>")
            )
          case _ =>
            node
              .childNodes()
              .forEach(childNode => fetchMethodInfoFromNode(filePath, childNode, currentNameSpace, moduleName))
        }
      }
    } catch {
      case ex: Exception =>
        logger.warn(s"Error while processing parsed AST for file -> ${filePath} : ", ex)
    }
  }

  // TODO: Add unit test for this method.
  def resolveModuleNameFromPath(path: String): String = {
    try {
      if (path.contains(tempLocation.toString)) {
        val moduleNameRegex = Seq("unpack", "([^", "]+)", "lib", ".*").mkString(java.io.File.separator).r
        moduleNameRegex
          .findFirstMatchIn(path)
          .map(_.group(1))
          .getOrElse("")
          .split(java.io.File.separator)
          .last
          .split("-")
          .init
          .mkString("-")
      } else {
        path
      }
    } catch {
      case ex: Exception =>
        logger.info(s"Error while fetching module name from $path path: ", ex)
        "Unknown"
    }
  }
  override def processCommand(command: String, item: Any, result: PackageTable): Unit = {
    try {
      command match
        case "ADD_TYPE_DECL" =>
          val typeDecl = item.asInstanceOf[TypeDeclMetaData]
          result.addTypeDecl(typeDecl.gemOrFileName, typeDecl.typeDeclName, typeDecl.typeDeclPath)
        case "ADD_MODULE" =>
          val module = item.asInstanceOf[ModuleMetaData]
          result.addModule(module.gemOrFileName, module.moduleName, module.modulePath)
        case "ADD_PACKAGE_METHOD" =>
          val packageMethodMetaData = item.asInstanceOf[PackageMethodMetaData]
          result.addPackageMethod(
            packageMethodMetaData.moduleName,
            packageMethodMetaData.methodName,
            packageMethodMetaData.parentClassPath,
            packageMethodMetaData.classType
          )
        case _ => logger.error(s"'${command}' is not handled")
    } catch {
      case ex: Exception =>
        logger.info(s"Error while processing ConcurrentProcessor Writer command '${command}' - item - ${item}", ex)
    }
  }
}
