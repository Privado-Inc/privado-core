package ai.privado.languageEngine.javascript.passes.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, JavaProperty}
import io.shiftleft.semanticcpg.language.*

class JSEnvPropertyLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[AstNode](cpg) {

  // Finds all assignment calls that assign a value to a property of the `process.env` object with the given
  // `propertyNode`
  def getMatchingLiteral: String = {
    // val g = config.DB_NAME
    // val g = process.env["DB_NAME"]
    // val g = process.env.DB_NAME
    s".*process\\.env(\\.|\\[('|`|\").*('|`|\")]).*|.*((config|Conf)\\.).*"
  }

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg
      .call("<operator>.(assignment|fieldAccess)")
      .astChildren
      .code(getMatchingLiteral)
      .filter(_.isCall)
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, node: AstNode): Unit = {
    if (node.asInstanceOf[Call].methodFullName == Operators.fieldAccess) {
      val propertyKey = node.code.split("\\.").last
      matchAndConnectPropertyNode(propertyKey, node, builder)
    } else if (node.asInstanceOf[Call].methodFullName == Operators.indexAccess) {
      val propertyKey = node.astChildren.isLiteral.head.code.replace("\"", "")
      matchAndConnectPropertyNode(propertyKey, node, builder)
    }
  }

  def connectEnvProperty(literalNode: AstNode, propertyNode: JavaProperty, builder: DiffGraphBuilder): Unit = {
    builder.addEdge(propertyNode, literalNode, EdgeTypes.IS_USED_AT)
    builder.addEdge(literalNode, propertyNode, EdgeTypes.ORIGINAL_PROPERTY)
  }

  private def matchAndConnectPropertyNode(propertyKey: String, node: AstNode, builder: DiffGraphBuilder): Unit = {
    cpg.property
      .filter(p => p.name.nonEmpty && p.value.nonEmpty && propertyKey.matches(p.name))
      .foreach(p => {
        connectEnvProperty(node, p, builder)
      })
  }
}
