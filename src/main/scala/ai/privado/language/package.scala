package ai.privado

import io.shiftleft.codepropertygraph.generated.{Cpg, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import overflowdb.traversal._

package object language {

  implicit class NodeStarters(cpg: Cpg) {
    def property: Traversal[JavaProperty] =
      cpg.graph.nodes(NodeTypes.JAVA_PROPERTY).cast[JavaProperty]
  }

}
