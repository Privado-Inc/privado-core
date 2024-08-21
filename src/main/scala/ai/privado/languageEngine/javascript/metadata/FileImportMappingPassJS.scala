package ai.privado.languageEngine.javascript.metadata

import ai.privado.cache.FileLinkingMetadata
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call

import java.util.regex.{Matcher, Pattern}
import java.io.{FileNotFoundException, File as JFile}
import better.files.File

import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Success, Try}

class FileImportMappingPassJS(cpg: Cpg, fileLinkingMetadata: FileLinkingMetadata)
    extends XImportResolverPass(cpg: Cpg) {

  private val pathPattern = Pattern.compile("[\"']([\\w/.]+)[\"']")

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val pathSep        = ":"
    val rawEntity      = importedEntity.stripPrefix("./")
    val alias          = importedAs
    val matcher        = pathPattern.matcher(rawEntity)
    val sep            = Matcher.quoteReplacement(JFile.separator)
    val root           = s"$codeRootDir${JFile.separator}"
    val currentFile    = s"$root$fileName"
    val extension      = File(currentFile).`extension`.getOrElse(".ts")
    val parentDirPath  = File(currentFile).parent.pathAsString
    val importedModule = importedEntity.split(pathSep).head
    // We want to know if the import is local since if an external name is used to match internal methods we may have
    // false paths.
    val isRelativeImport = importedEntity.matches("^[.]+/?.*")
    // TODO: At times there is an operation inside of a require, e.g. path.resolve(__dirname + "/../config/env/all.js")
    //  this tries to recover the string but does not perform string constant propagation
    val entity = if (matcher.find()) matcher.group(1) else rawEntity

    if (isRelativeImport) {
      getResolvedPath(root, parentDirPath, importedModule, importedAs) match
        case Failure(_)            => // unable to resolve
        case Success(resolvedPath) => fileLinkingMetadata.addToFileImportMap(fileName, resolvedPath)
    } else {

      val relativeDirCount = parentDirPath.stripPrefix(root).split(sep).size
      breakable {
        for (i <- 0 to relativeDirCount) {
          val resolvedPath =
            getResolvedPath(root, parentDirPath.split(sep).dropRight(i).mkString(sep), importedModule, importedAs)
          if (resolvedPath.isSuccess) {
            fileLinkingMetadata.addToFileImportMap(fileName, resolvedPath.get)
            break
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
        // If not found, try to find a file with the same name but with any extension
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

}
