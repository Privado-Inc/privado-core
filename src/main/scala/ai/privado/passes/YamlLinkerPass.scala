package ai.privado.passes

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.shiftleft.semanticcpg.language.*

class YamlLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

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
    matchingLiteralsInGetEnvCalls(propertyNode.name).foreach(lit => {
      builder.addEdge(propertyNode, lit, EdgeTypes.IS_USED_AT)
      builder.addEdge(lit, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
    })
  }

  private def matchingLiteralsInGetEnvCalls(propertyName: String): List[Literal] = {
    val propertyKey = propertyName.split("\\.").last
    cpg.literal
      .codeExact("\"" + propertyKey + "\"")
      .filter(_.inCall.name("(?i).*getenv").nonEmpty)
      .toList
  }
}
