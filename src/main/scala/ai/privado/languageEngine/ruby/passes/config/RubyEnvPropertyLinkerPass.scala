package ai.privado.languageEngine.ruby.passes.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.passes.PropertyEnvLinkerPassBase
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, JavaProperty, Literal}
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

class RubyEnvPropertyLinkerPass(cpg: Cpg) extends PrivadoParallelCpgPass[AstNode](cpg) {

  def getMatchingLiteral: String = {
    // Examples:
    // val name = ENV.fetch["DB_NAME"]
    // val name = "#{Settings.foo.DB_NAME}/bar"
    s".*ENV(\\.fetch)?\\[('|`|\").*('|`|\")].*|.*((Settings)\\.).*"
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
    if (node.asInstanceOf[Call].methodFullName == Operators.indexAccess) {
      val propertyKey = node.astChildren.isLiteral.head.code.replace("\"", "")
      matchAndConnectPropertyNode(propertyKey, node, builder)
    } else if (node.astChildren.isCall.nonEmpty) {
      val formattedValue = node.astChildren.isCall.methodFullName("<operator>.formatValue").head.code
      val captureRegex   = "#\\{Settings\\.(.*?)}".r
      val propertyKey    = captureRegex.findFirstMatchIn(formattedValue).map(_.group(1)).getOrElse("")
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
