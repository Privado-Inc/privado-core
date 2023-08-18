package ai.privado.languageEngine.ruby.passes

import ai.privado.tagger.PrivadoParallelCpgPass
import io.joern.rubysrc2cpg.utils.PackageTable
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
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
    .filter(_.argument.headOption.exists(_.isCall))
    .filter(_.argument.head.asInstanceOf[Call].name.equals("<operator>.scopeResolution"))
    .toArray

  def runOnPart(builder: DiffGraphBuilder, callNode: Call): Unit = {

    val callClassName = callNode.argument.head.ast.isIdentifier.map(_.name).mkString(".")

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

  private def updateDynamicTypeHintFullName(builder: DiffGraphBuilder, callNode: Call, fullNames: List[String]) = {
    builder.setNodeProperty(
      callNode,
      PropertyNames.DynamicTypeHintFullName,
      (callNode.dynamicTypeHintFullName ++ fullNames).distinct
    )
  }

}
