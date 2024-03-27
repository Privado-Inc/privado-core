package ai.privado.languageEngine.go.passes.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.passes.PropertyEnvLinkerPassBase
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, JavaProperty, Literal}

class GoEnvPropertyLinkerPass(cpg: Cpg) extends PropertyEnvLinkerPassBase(cpg) {

  // List all literal fetching property value
  // ex: os.getenv("KEY")
  override def getMatchingLiteral: String = {
    "(?i).*getenv"
  }

  override def connectProperties(node: AstNode, builder: DiffGraphBuilder): Unit = {
    if (node.isInstanceOf[Literal]) {
      val propertyValue = getPropertyKeyFromEnvCall(node)
      cpg.property
        .filter(p => p.name.nonEmpty && p.value.nonEmpty && propertyValue.matches(p.name))
        .foreach(p => {
          connectEnvProperty(node, p, builder)
        })
    }
  }
}
