package ai.privado.languageEngine.python.passes

import io.joern.pysrc2cpg.PythonTypeHintCallLinker
import io.joern.x2cpg.passes.frontend.XTypeRecovery.isDummyType
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, MethodBase}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, PropertyNames}

class PrivadoPythonTypeHintCallLinker(cpg: Cpg) extends PythonTypeHintCallLinker(cpg) {

  override protected def linkCallsToCallees(
                                             callerAndCallees: List[(Call, Seq[String])],
                                             methodMap: Map[String, MethodBase],
                                             builder: DiffGraphBuilder
                                           ): Unit = {
    // Link edges to method nodes
    callerAndCallees.foreach { case (call, methodNames) =>
      methodNames
        .flatMap(methodMap.get)
        .foreach { m => builder.addEdge(call, m, EdgeTypes.CALL) }
      if (methodNames.size == 1) {
        builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, methodNames.head)
      } else if (methodNames.size > 1) {
        val nonDummyMethodNames = methodNames.filterNot(isDummyType)
        if (nonDummyMethodNames.nonEmpty) {
          builder.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, nonDummyMethodNames.minBy(_.length))
        }
      }
    }
  }

}
