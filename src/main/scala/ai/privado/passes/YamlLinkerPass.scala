package ai.privado.passes

import ai.privado.semantic.language.*
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.shiftleft.semanticcpg.language.*

abstract class YamlLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  private val YAML_FILE_REGEX: String = ".*(.yaml)"

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property
      .where(_.file.name(YAML_FILE_REGEX))
      .iterator
      .filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    connectProperty(property, builder)
  }

  private def connectProperty(propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    matchingLiteralsToPropertyNode(propertyNode.name).foreach(lit => {
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
      builder.addEdge(lit, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

  def matchingLiteralsToPropertyNode(propertyName: String): List[Literal]
}
