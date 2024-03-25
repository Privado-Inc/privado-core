package ai.privado.languageEngine.python.passes.config

import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call}
import overflowdb.BatchedUpdate
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.InternalTag
import ai.privado.passes.PropertyEnvLinkerPassBase
import io.shiftleft.semanticcpg.language.*

class PythonEnvPropertyLinkerPass(cpg: Cpg) extends PropertyEnvLinkerPassBase(cpg) {

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.literal
      .filter(_.inCall.methodFullName(getMatchingLiteral).nonEmpty)
      .toArray
  }

  override def getMatchingLiteral: String = {
    // Ex:
    // os.environ.get("DB_NAME")
    ".*\\(?environ\\)?\\.get.*"
  }

  override def connectProperties(node: AstNode, builder: DiffGraphBuilder): Unit = {
    val propertyKey = node.code.replace("\"", "")
    cpg.property
      .whereNot(_.tag.name(InternalTag.SOURCE_PROPERTY.toString))
      .filter(p => p.name.nonEmpty && p.value.nonEmpty && propertyKey.matches(p.name))
      .foreach(p => {
        connectEnvProperty(node, p, builder)
      })
  }
}
