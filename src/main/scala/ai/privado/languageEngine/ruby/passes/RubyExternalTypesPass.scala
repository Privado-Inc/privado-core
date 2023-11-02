package ai.privado.languageEngine.ruby.passes

import ai.privado.tagger.PrivadoParallelCpgPass
import io.joern.rubysrc2cpg.deprecated.utils.PackageTable
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.semanticcpg.language.*

class RubyExternalTypesPass(cpg: Cpg, packageTable: PackageTable) extends PrivadoParallelCpgPass[Call](cpg) {

  val moduleMapping: Map[String, List[(String, String)]] = packageTable.moduleMapping.values
    .flatMap(moduleMappings =>
      moduleMappings.map { module =>
        val splittedFullName = module.fullName.split("::program.").toList
        (splittedFullName.last, splittedFullName.head)
      }
    )
    .toList
    .groupBy(_._1)

  val typeDeclMapping: Map[String, List[(String, String)]] = packageTable.typeDeclMapping.values
    .flatMap(typeDeclMappings =>
      typeDeclMappings.map { typeDecl =>
        val splittedFullName = typeDecl.fullName.split("::program.").toList
        (splittedFullName.last, splittedFullName.head)
      }
    )
    .toList
    .groupBy(_._1)

  def generateParts(): Array[Call] = cpg.call
    .whereNot(_.nameExact("<operator>.scopeResolution"))
    .toArray

  def runOnPart(builder: DiffGraphBuilder, callNode: Call): Unit = {

    val callClassName = {
      if (
        callNode.argument.headOption
          .exists(_.isCall) && callNode.argument.head.asInstanceOf[Call].name.equals("<operator>.scopeResolution")
      ) {
        callNode.argument.head.ast.isIdentifier.map(_.name).mkString(".")
      } else if (callNode.argument.argumentIndex(0).nonEmpty && callNode.argument.headOption.exists(_.isIdentifier)) {
        callNode.argument.head.asInstanceOf[Identifier].name
      } else
        ""
    }

    if (callClassName.nonEmpty) {
      moduleMapping.get(callClassName) match
        case Some(matchingModules) =>
          val types = matchingModules.map(module => s"${module._2}::program.${module._1}.${callNode.name}")
          updateDynamicTypeHintFullName(builder, callNode, types)
        case None =>

      typeDeclMapping.get(callClassName) match
        case Some(matchingTypeDecl) =>
          val types = matchingTypeDecl.map(typeDecl => s"${typeDecl._2}::program.${typeDecl._1}.${callNode.name}")
          updateDynamicTypeHintFullName(builder, callNode, types)
        case None =>
    }

  }

  private def updateDynamicTypeHintFullName(builder: DiffGraphBuilder, callNode: Call, fullNames: List[String]) = {
    builder.setNodeProperty(
      callNode,
      PropertyNames.DynamicTypeHintFullName,
      (callNode.dynamicTypeHintFullName ++ fullNames).distinct
    )
  }

}
