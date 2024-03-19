package ai.privado.passes

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.shiftleft.codepropertygraph.generated.nodes.*
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, Node}

/** This pass creates a graph layer for env call from property node
  */

abstract class PropertyEnvLinkerPassBase(cpg: Cpg) extends PrivadoParallelCpgPass[JavaProperty](cpg) {

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.property.iterator
      .filter(pair => pair.name.nonEmpty && pair.value.nonEmpty)
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, property: JavaProperty): Unit = {
    connectProperties(property, builder)
  }

  def connectProperties(property: JavaProperty, builder: DiffGraphBuilder): Unit

  // This method used to connect the property Node with literal where it used
  def connectEnvProperty(literalNode: Node, propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    builder.addEdge(propertyNode, literalNode, EdgeTypes.IS_USED_AT)
    builder.addEdge(literalNode, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
  }
}
