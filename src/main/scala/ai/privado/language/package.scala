package ai.privado

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, File, JavaProperty, Literal}
import overflowdb.traversal._

package object language {

  implicit class NodeStarters(cpg: Cpg) {
    def property: Traversal[JavaProperty] =
      cpg.graph.nodes(NodeTypes.JAVA_PROPERTY).cast[JavaProperty]
  }

  implicit class StepsForProperty(val trav: Traversal[JavaProperty]) extends AnyVal {

    def usedAt: Traversal[CfgNode] = trav.out(EdgeTypes.IS_USED_AT).cast[CfgNode]
    def file: Traversal[File]      = trav.out(EdgeTypes.SOURCE_FILE).cast[File]

  }

}
