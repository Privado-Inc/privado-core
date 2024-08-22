package ai.privado.languageEngine.javascript.metadata

import ai.privado.cache.{AppCache, FileLinkingMetadata}
import ai.privado.passes.{JsonParser, Source}
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call

import java.util.regex.{Matcher, Pattern}
import java.io.{FileNotFoundException, File as JFile}
import better.files.File
import better.files.File.VisitOptions
import io.joern.x2cpg.SourceFiles

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Success, Try}

class FileImportMappingPassJS(cpg: Cpg, fileLinkingMetadata: FileLinkingMetadata, appCache: AppCache)
    extends XImportResolverPass(cpg: Cpg)
    with JsonParser {

  private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  private val tsConfigPathMapping = mutable.HashMap[String, String]()

  private val tsConfigEntityMissCache = mutable.HashSet[String]()

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val pathSep       = ":"
    val rawEntity     = importedEntity.stripPrefix("./")
    val alias         = importedAs
    val matcher       = pathPattern.matcher(rawEntity)
    val sep           = Matcher.quoteReplacement(JFile.separator)
    val root          = s"${codeRootDir.replace(s"${sep}probe$sep", s"$sep")}${JFile.separator}"
    val currentFile   = s"$root$fileName"
    val extension     = File(currentFile).`extension`.getOrElse(".ts")
    val parentDirPath = File(currentFile).parent.pathAsString
    // initialize tsconfig.json map
    initializeConfigMap(sep)
    val importedModule = getImportingModule(importedEntity, pathSep)

    // We want to know if the import is local since if an external name is used to match internal methods we may have
    // false paths.
    val isRelativeImport = importedEntity.matches("^[.]+/?.*")

    if (isRelativeImport && importedModule.isDefined) {
      getResolvedPath(root, parentDirPath, importedModule.get, importedAs) match
        case Failure(_)            => // unable to resolve
        case Success(resolvedPath) => fileLinkingMetadata.addToFileImportMap(fileName, resolvedPath)
    } else if (importedModule.isDefined) {
      val relativeDirCount = parentDirPath.stripPrefix(root).split(sep).length
      breakable {
        for (i <- 0 to relativeDirCount) {
          val resolvedPath =
            getResolvedPath(root, parentDirPath.split(sep).dropRight(i).mkString(sep), importedModule.get, importedAs)
          if (resolvedPath.isSuccess) {
            fileLinkingMetadata.addToFileImportMap(fileName, resolvedPath.get)
            println(s"resolving success for ${importedModule.get}, $importedAs at parentDirPath : ${parentDirPath}")
            break
          } else {
            println(s"resolving failed for ${importedModule.get}, $importedAs at parentDirPath : ${parentDirPath}")
          }
        }
      }
    }
  }

  def getResolvedPath(root: String, parentDirPath: String, relativePath: String, importedAs: String): Try[String] =
    Try {
      val file = File(parentDirPath, relativePath)
      if (file.exists) {
        if (file.isDirectory) {
          val fileWithSameName = file.listRecursively.find { f =>
            f.isRegularFile && f.nameWithoutExtension == importedAs
          }
          if (fileWithSameName.isDefined)
            fileWithSameName.get.pathAsString.stripPrefix(root)
          else
            throw FileNotFoundException()
        } else file.pathAsString.stripPrefix(root)
      } else {
        // If not found, try to find a file with the same name extension doesn't matter
        val baseName  = file.nameWithoutExtension
        val parentDir = file.parent
        val fileWithSameName = parentDir.list.find { f =>
          f.isRegularFile && f.nameWithoutExtension == baseName
        }
        if (fileWithSameName.isDefined)
          fileWithSameName.get.pathAsString.stripPrefix(root)
        else
          throw FileNotFoundException()
      }
    }

  private def getImportingModule(importedEntity: String, pathSep: String) = {
    importedEntity.split(pathSep).head match
      case entity if entity.startsWith("@") =>
        // if import starts with `@` this can mean import of local modules in some case
        if (tsConfigPathMapping.contains(entity)) {
          Some(tsConfigPathMapping(entity))
        } else if (tsConfigEntityMissCache.contains(entity))
          None
        else {
          tsConfigPathMapping.keys.filter(_.endsWith("*")).find { k =>
            val keyRegex = k.replace("*", ".*").r
            val value    = keyRegex.matches(entity)
            value
          } match
            case Some(configKey) =>
              val configPathValue = tsConfigPathMapping(configKey).stripSuffix("*")
              val resolvedModule  = entity.replace(configKey.stripSuffix("*"), configPathValue)
              // println(s"ResolvedModule : $resolvedModule, for $entity and $importedEntity")
              Some(resolvedModule)
            case None =>
              println(s"Not able to resolve : $entity, $importedEntity")
              tsConfigEntityMissCache.add(entity)
              None
        }
      case entity => Some(entity)
  }

  private def getJsonPathConfigFiles(sep: String): List[String] = {
    val repoPath = appCache.scanPath.replace(s"${sep}probe${sep}", s"${sep}")
    val filePaths =
      if (appCache.excludeFileRegex.isDefined)
        SourceFiles
          .determine(repoPath, Set(".json"), ignoredFilesRegex = Option(appCache.excludeFileRegex.get.r))(
            VisitOptions.default
          )
      else
        SourceFiles.determine(repoPath, Set(".json"))(VisitOptions.default)

    val filteredFilePaths = filePaths.filter { fp =>
      val f = File(fp)
      f.nameWithoutExtension.contains("tsconfig") || f.nameWithoutExtension.contains("jsconfig")
    }
    filteredFilePaths
  }

  private def initializeConfigMap(sep: String): Unit = {
    val configFilePaths = getJsonPathConfigFiles(sep)

    configFilePaths.foreach { configFilePath =>
      getJSONKeyValuePairs(configFilePath)
        .filter(_._1.contains("compilerOptions.paths"))
        .foreach { pathEntry =>
          // do clean up of the paths key
          // We would get keys like - compilerOptions.paths.@utils/*[0]
          val pathKey   = pathEntry._1.split("compilerOptions.paths.").last.split("\\[").head
          val pathValue = pathEntry._2
        tsConfigPathMapping.addOne(pathKey, pathValue)
        }
    }
  }
}
