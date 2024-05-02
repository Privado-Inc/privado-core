package ai.privado.languageEngine

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.HightouchSink
import overflowdb.traversal.*

import scala.jdk.CollectionConverters.IteratorHasAsScala

package object default {
  implicit class NodeStarters(cpg: Cpg) {
    def highTouchSink: Traversal[HightouchSink] =
      cpg.graph.nodes(NodeTypes.HIGHTOUCH_SINK).asScala.cast[HightouchSink]
  }
}
