package ai.privado.joern

import io.shiftleft.codepropertygraph.generated.nodes.Mynodetype
import io.shiftleft.codepropertygraph.generated.{Cpg, NodeTypes}
import overflowdb.traversal._

package object language {

  /** Example of a custom language step
    */
  implicit class MynodetypeSteps(val traversal: Traversal[Mynodetype]) extends AnyVal {
    def myCustomStep: Traversal[Mynodetype] = {
      println("custom step executed")
      traversal
    }
  }

  /** Example implicit conversion that forwards to the `StandaloneStarters` class
    */
  implicit def toStandaloneStarters(cpg: Cpg): StandaloneStarters =
    new StandaloneStarters(cpg)
}

/** Example of custom node type starters
  */
class StandaloneStarters(cpg: Cpg) {
  def mynodetype: Traversal[Mynodetype] =
    cpg.graph.nodes(NodeTypes.MYNODETYPE).cast[Mynodetype]
}
