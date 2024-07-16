package ai.privado.languageEngine.c.passes

import io.joern.x2cpg.passes.frontend.XImportResolverPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.importresolver.*
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Import}
import io.shiftleft.semanticcpg.language.*

class CImportResolverPass(cpg: Cpg) extends XImportResolverPass(cpg) {

  override def runOnPart(builder: DiffGraphBuilder, part: Import): Unit = for {
    importedAs     <- part.importedAs
    importedEntity <- part.importedEntity
  } {
    val fileName = part.file.name.headOption.getOrElse("<unknown>").stripPrefix(codeRootDir)
    optionalResolveImportNode(fileName, part, importedEntity, importedAs, builder)
  }

  override protected def optionalResolveImport(
    fileName: String,
    importCall: Call,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val operation = "donothing"
  }

  private def optionalResolveImportNode(
    fileName: String,
    importNode: Import,
    importedEntity: String,
    importedAs: String,
    diffGraph: DiffGraphBuilder
  ): Unit = {
    val fileDepth = importedEntity.split("/").size
    val headerFileName = (fileName.split("/").dropRight(fileDepth) match {
      case x if x.isEmpty => ""
      case x              => x.mkString("/").concat("/")
    }).concat(s"$importedEntity")

    resolveEntity(headerFileName, importNode).foreach(x => evaluatedImportNodeToTag(x, importNode, diffGraph))
  }

  private def resolveEntity(headerFileName: String, importNode: Import) = {
    val methods =
      importNode.file.method.where(_.file.nameExact(headerFileName)).map(m => ResolvedMethod(m.fullName, m.name)).l

    val typeDecls = importNode.file.typeDecl
      .where(_.file.nameExact(headerFileName))
      .map(typeDecl => ResolvedTypeDecl(typeDecl.fullName))
      .l
    val tmp = (typeDecls ++ methods).collectAll[EvaluatedImport].toSet
    tmp
  }

  private def evaluatedImportNodeToTag(x: EvaluatedImport, importNode: Import, diffGraph: DiffGraphBuilder): Unit =
    importNode.start.newTagNodePair(x.label, x.serialize).store()(diffGraph)
}
