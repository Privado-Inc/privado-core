package ai.privado

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, File, JavaProperty, Literal, MethodParameterIn}
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

  implicit class NodeTravToProperty(val trav: Traversal[CfgNode]) {
    def originalProperty: Traversal[JavaProperty] = trav.out(EdgeTypes.ORIGINAL_PROPERTY).cast[JavaProperty]
  }

  implicit class NodeToProperty(val node: CfgNode) {
    def originalProperty: Option[JavaProperty] = {
      val property = node.out(EdgeTypes.ORIGINAL_PROPERTY)
      if (property != null && property.hasNext) {
        val prop = property.next()
        if (prop.isInstanceOf[JavaProperty]) {
          return Some(prop.asInstanceOf[JavaProperty])
        }
      }
      None
    }
    def originalPropertyValue: Option[String] = {
      val property = node.out(EdgeTypes.ORIGINAL_PROPERTY)
      if (property != null && property.hasNext) {
        val prop = property.next()
        if (prop.isInstanceOf[JavaProperty]) {
          return Some(prop.asInstanceOf[JavaProperty].value)
        }
      }
      None
    }
  }
}
