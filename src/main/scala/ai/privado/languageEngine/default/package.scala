package ai.privado.languageEngine

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.HightouchSink

import scala.jdk.CollectionConverters.IteratorHasAsScala

package object default {
  implicit class NodeStarters(cpg: Cpg) {
    def highTouchSink: Iterator[HightouchSink] =
      cpg.graph.nodes(NodeTypes.HIGHTOUCH_SINK).asScala.cast[HightouchSink]
  }
}
