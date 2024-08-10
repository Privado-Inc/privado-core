package ai.privado.languageEngine.javascript.metadata

import ai.privado.cache.FileLinkingMetadata
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call

import java.util.regex.{Matcher, Pattern}
import java.io.File as JFile
import scala.util.Try

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
    val pathSep     = ":"
    val rawEntity   = importedEntity.stripPrefix("./")
    val alias       = importedAs
    val matcher     = pathPattern.matcher(rawEntity)
    val sep         = Matcher.quoteReplacement(JFile.separator)
    val root        = s"$codeRootDir${JFile.separator}"
    val currentFile = s"$root$fileName"
    val extension   = better.files.File(currentFile).`extension`.getOrElse(".ts")
    // We want to know if the import is local since if an external name is used to match internal methods we may have
    // false paths.
    val isLocalImport = importedEntity.matches("^[.]+/?.*")
    // TODO: At times there is an operation inside of a require, e.g. path.resolve(__dirname + "/../config/env/all.js")
    //  this tries to recover the string but does not perform string constant propagation
    val entity = if (matcher.find()) matcher.group(1) else rawEntity

    val isImportingModule = !entity.contains(pathSep)
    if (isLocalImport) {
      val resolvedPath = Try(
        better.files
          .File(currentFile.stripSuffix(currentFile.split(sep).last), entity.split(pathSep).head)
          .pathAsString
          .stripPrefix(root)
      ).getOrElse(entity)
      fileLinkingMetadata.addToFileImportMap(fileName, s"$resolvedPath$extension")
    } else {
      val seperatedFilePathList = fileName.split(sep).toList
      val startingModule        = entity.split(sep).head
      val moduleIndex           = seperatedFilePathList.indexOf(startingModule)
      if (moduleIndex != -1) {
        Try {
          val resolvedPath = better.files
            .File(root, seperatedFilePathList.take(moduleIndex).mkString(sep), entity.split(pathSep).head)
            .pathAsString
            .stripPrefix(root)
          fileLinkingMetadata.addToFileImportMap(fileName, s"$resolvedPath$extension")
        }
      }

    }

  }

}
