package ai.privado.languageEngine.python.metadata

import better.files.File
import ai.privado.cache.FileLinkingMetadata
import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Member, Method, TypeDecl}
import io.shiftleft.semanticcpg.language.*

import java.util.regex.Matcher
import java.io.File as JFile
import scala.collection.mutable

class FileLinkingMetadataPassPython(cpg: Cpg, fileLinkingMetadata: FileLinkingMetadata)
    extends XImportResolverPass(cpg) {

  private val moduleCache: mutable.HashMap[String, ImportableEntity] = mutable.HashMap.empty

  override def init(): Unit = {
    cpg.typeDecl.isExternal(false).nameExact("<module>").foreach { moduleType =>
      val modulePath = fileToPythonImportNotation(moduleType.filename)
      cpg.method.fullNameExact(moduleType.fullName).headOption.foreach { moduleMethod =>
        moduleCache.put(modulePath, Module(moduleType, moduleMethod))
        moduleMethod.astChildren.foreach {
          case moduleFunction: Method =>
            moduleCache.put(s"$modulePath.${moduleFunction.name}", ImportableFunction(moduleFunction))
          // Ignore types for functions that are used for method pointers
          case moduleType: TypeDecl if moduleMethod.astChildren.isMethod.fullNameExact(moduleType.fullName).isEmpty =>
            moduleCache.put(s"$modulePath.${moduleType.name}", ImportableType(moduleType))
          case _ => // do nothing
        }
      }
      moduleType.member.foreach { moduleMember =>
        moduleCache
          .getOrElseUpdate(s"$modulePath.${moduleMember.name}", ModuleVariable(moduleType.fullName, moduleMember))
      }
    }
  }

  private def fileToPythonImportNotation(filename: String): String =
    filename
      .stripPrefix(codeRootDir)
      .replaceAll(Matcher.quoteReplacement(JFile.separator), ".")
      .stripSuffix(".py")
      .stripSuffix(".__init__")

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val currDir = File(codeRootDir) / fileName match
      case x if x.isDirectory => x
      case x                  => x.parent

    val importedEntityAsFullyQualifiedImport =
      // If the path/entity uses Python's `from .import x` syntax, we will need to remove these
      fileToPythonImportNotation(importedEntity.replaceFirst("^\\.+", ""))
    val importedEntityAsRelativeImport = Seq(
      fileToPythonImportNotation(currDir.pathAsString.stripPrefix(codeRootDir).stripPrefix(JFile.separator)),
      importedEntityAsFullyQualifiedImport
    ).filterNot(_.isBlank).mkString(".")

    // We evaluated both variations, based on what we could expect from different versions of Python and how the package
    // layout is interpreted by the presence of lack of `__init__.py` files. Additionally, external packages are always
    // fully qualified.
    val resolvedImports =
      Seq(
        moduleCache.get(importedEntityAsRelativeImport),
        moduleCache.get(importedEntityAsFullyQualifiedImport)
      ).flatten.foreach(_.toResolvedImport(fileName: String))
  }

  private sealed trait ImportableEntity {

    def toResolvedImport(fileName: String): Unit

  }

  private case class Module(moduleType: TypeDecl, moduleMethod: Method) extends ImportableEntity {
    override def toResolvedImport(fileName: String): Unit = {
      fileLinkingMetadata.addToFileImportMap(fileName, moduleType.filename)
      fileLinkingMetadata.addToFileImportMap(fileName, moduleMethod.filename)
    }

  }

  private case class ModuleVariable(baseTypeFullName: String, member: Member) extends ImportableEntity {

    override def toResolvedImport(fileName: String): Unit =
      fileLinkingMetadata.addToFileImportMap(fileName, member.file.name.head)
  }

  private case class ImportableFunction(function: Method) extends ImportableEntity {
    override def toResolvedImport(fileName: String): Unit =
      fileLinkingMetadata.addToFileImportMap(fileName, function.filename)
  }

  private case class ImportableType(typ: TypeDecl) extends ImportableEntity {
    override def toResolvedImport(fileName: String): Unit =
      fileLinkingMetadata.addToFileImportMap(fileName, typ.filename)
  }
}
