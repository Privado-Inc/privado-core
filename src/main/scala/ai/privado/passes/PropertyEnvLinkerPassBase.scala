package ai.privado.passes

import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.{BatchedUpdate, Node}

/** This pass creates a graph layer for env call from property node
  */

abstract class PropertyEnvLinkerPassBase(cpg: Cpg) extends PrivadoParallelCpgPass[AstNode](cpg) {

  val logger = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.literal
      .filter(_.inCall.name(getMatchingLiteral).nonEmpty)
      .toArray
  }

  def getMatchingLiteral: String

  override def runOnPart(builder: DiffGraphBuilder, node: AstNode): Unit = {
    connectProperties(node, builder)
  }

  def connectProperties(node: AstNode, builder: DiffGraphBuilder): Unit

  // This method used to connect the property Node with literal where it used
  def connectEnvProperty(literalNode: AstNode, propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    builder.addEdge(propertyNode, literalNode, EdgeTypes.IS_USED_AT)
    builder.addEdge(literalNode, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
  }

  // return Property Key from Call
  // Ex: "DB_VALUE" => DB_VALUE
  def getPropertyKeyFromEnvCall(node: AstNode): String = {
    node.code.replace("\"", "")
  }
}
