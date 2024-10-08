package ai.privado.languageEngine.javascript.metadata

import ai.privado.cache.{AppCache, FileLinkingMetadata, RuleCache}
import ai.privado.passes.JsonParser
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call

import java.util.regex.{Matcher, Pattern}
import java.io.{FileNotFoundException, File as JFile}
import better.files.File
import better.files.File.VisitOptions
import io.joern.x2cpg.SourceFiles

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Success, Try}

class FileImportMappingPassJS(
  cpg: Cpg,
  fileLinkingMetadata: FileLinkingMetadata,
  appCache: AppCache,
  ruleCache: RuleCache
) extends XImportResolverPass(cpg: Cpg)
    with JsonParser {

  private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  val sep: String = Matcher.quoteReplacement(JFile.separator)
  val root        = s"${sanitiseProbeScanPath(codeRootDir)}${JFile.separator}"

  private val tsConfigPathMapping = mutable.HashMap[String, String]()

  private val tsConfigEntityMissCache = ConcurrentHashMap.newKeySet[String]()

  override def init(): Unit = {
    // initialize tsconfig.json map
    initializeConfigMap()
  }

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val pathSep       = ":"
    val currentFile   = s"$root$fileName"
    val extension     = File(currentFile).`extension`.getOrElse(".ts")
    val parentDirPath = File(currentFile).parent.pathAsString // Stores the path of the parent dir of current file

    val importedModule = getImportingModule(importedEntity, pathSep)

    // We want to know if the import is local since if an external name is used to match internal methods we may have
    // false paths.
    val isRelativeImport = importedEntity.matches("^[.]+/?.*")

    if (isRelativeImport && importedModule.isDefined) {
      getResolvedPath(parentDirPath, importedModule.get, importedAs, extension) match
        case Failure(_)            => // unable to resolve
        case Success(resolvedPath) => fileLinkingMetadata.addToFileImportMap(fileName, resolvedPath)
    } else if (importedModule.isDefined) {
      val relativeDirCount = parentDirPath.stripPrefix(root).split(sep).length
      breakable {
        for (i <- 0 to relativeDirCount) {
          val resolvedPath =
            getResolvedPath(
              parentDirPath.split(sep).dropRight(i).mkString(sep),
              importedModule.get,
              importedAs,
              extension
            )
          if (resolvedPath.isSuccess) {
            fileLinkingMetadata.addToFileImportMap(fileName, resolvedPath.get)
            break
          }
        }
      }
    }
  }

  /** Function to get us a a probable relative file path, if exists for the importing module based on input parameters
    * @param parentDirPath
    *   \- parent dir path where we intend to do the lookup
    * @param relativePath
    *   \- importing module path
    * @param importedAs
    *   \- importedAs value of import
    * @param currentFileExtension
    *   \- current extension of the file being processed
    * @return
    */
  def getResolvedPath(
    parentDirPath: String,
    relativePath: String,
    importedAs: String,
    currentFileExtension: String
  ): Try[String] =
    Try {
      val file = File(parentDirPath, relativePath)
      if (file.exists && file.isRegularFile) {
        file.pathAsString.stripPrefix(root)
      } else {
        // If not found, try to find a file with the same name extension
        val baseName  = file.nameWithoutExtension
        val parentDir = file.parent
        val fileWithSameNames = parentDir.list.filter { f =>
          f.isRegularFile && f.nameWithoutExtension == baseName
        }.toList

        // If multiple files match with sameName, prefer the one having same extension
        fileWithSameNames.size match
          case size if size == 0 => throw FileNotFoundException()
          case size if size == 1 => fileWithSameNames.head.pathAsString.stripPrefix(root)
          case _ =>
            fileWithSameNames.find(f => f.`extension`.exists(_.equals(currentFileExtension))) match
              case Some(fileWithSameNameAndExtension) => fileWithSameNameAndExtension.pathAsString.stripPrefix(root)
              case None                               => fileWithSameNames.head.pathAsString.stripPrefix(root)
      }
    }

  /** From ImportedEntity after applying some lookup gives the importing Module
    * @param importedEntity
    *   \- The value present as part of import or require statement
    * @param pathSep
    *   \- The path sep used to concat the importing modules elements
    * @return
    */
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
              tsConfigEntityMissCache.add(entity)
              None
        }
      case entity => Some(entity)
  }

  /** Returns all the file paths where tsconfig.json or jsconfig.json are defined
    * @return
    */
  private def getJsonPathConfigFiles: List[String] = {
    val repoPath = sanitiseProbeScanPath(appCache.scanPath)
    val filePaths = SourceFiles
      .determine(repoPath, Set(".json"), ignoredFilesRegex = Option(ruleCache.getExclusionRegex.r))(
        VisitOptions.default
      )

    val filteredFilePaths = filePaths.filter { fp =>
      val f = File(fp)
      f.nameWithoutExtension.contains("tsconfig") || f.nameWithoutExtension.contains("jsconfig")
    }
    filteredFilePaths
  }

  /** Initializes the configuration map by reading the configurations files
    */
  private def initializeConfigMap(): Unit = {
    val compilerPathConstant = "compilerOptions.paths"
    val configFilePaths      = getJsonPathConfigFiles

    configFilePaths.foreach { configFilePath =>
      getJSONKeyValuePairs(configFilePath)
        .filter(_._1.contains(compilerPathConstant))
        .foreach { pathEntry =>
          // do clean up of the paths key
          // We would get keys like - compilerOptions.paths.@utils/*[0]
          val pathKey   = pathEntry._1.split(s"${compilerPathConstant}.").last.split("\\[").head
          val pathValue = pathEntry._2
          tsConfigPathMapping.addOne(pathKey, pathValue)
        }
    }
  }

  /** Function to sanitise the scanPath and remove `/probe` as we copy files in /probe folder, this helps in lookup for
    * resolvedPaths
    * @param scanPath
    *   \- repo path used for scanning
    * @return
    */
  private def sanitiseProbeScanPath(scanPath: String) = scanPath.replace(s"${sep}probe", "")
}
