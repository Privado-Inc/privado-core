package ai.privado.languageEngine.ruby.passes

import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Local, Method, NewCall}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.collection.mutable

/** Scans for identifiers that are meant to be calls but are simply invoked without parenthesis'. Uses heuristics here
  * and there to make the distinction.
  */
class IdentifierToCallPassPrivado(cpg: Cpg) extends ConcurrentWriterCpgPass[Method](cpg) {

  private val methodNameMap: Map[String, List[Method]] = cpg.method.groupBy(_.name)
  private val methodNameSet: Set[String]               = methodNameMap.keySet

  private val removedEdge    = mutable.HashSet[overflowdb.Edge]()
  private val removedNodeMap = mutable.HashMap[Long, overflowdb.Node]()

  override def runWithBuilder(externalBuilder: BatchedUpdate.DiffGraphBuilder): Int = {
    val returnValue = super.runWithBuilder(externalBuilder)
    removedEdge.foreach(externalBuilder.removeEdge)
    removedNodeMap.values.foreach(node => externalBuilder.removeNode(node))
    returnValue
  }

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, part: Method): Unit =
    part.local.referencingIdentifiers
      .filter(sharesNameWithMethod)
      .whereNot(isTargetOfAssignment)
      .refsTo
      .collectAll[Local]
      .foreach(node => convertToCall(diffGraph, node))

  /** @return
    *   true if the call shares a name with a defined method, false if otherwise.
    */
  private def sharesNameWithMethod(node: Identifier): Boolean =
    methodNameSet.contains(node.name)

  /** Determines if the identifier is the LHS of an assignment.
    */
  private def isTargetOfAssignment(nodes: Iterator[Identifier]): Iterator[Identifier] =
    if (nodes.nonEmpty) nodes.where(_.inAssignment).argumentIndex(1)
    else nodes

  /** Removes the local node and replaces all if its referencing identifiers with call nodes.
    */
  private def convertToCall(diffGraph: DiffGraphBuilder, node: Local): Unit = {
    node.referencingIdentifiers.foreach { i =>
      // Create call with not much type info - this will be handled in type propagation
      val call = NewCall()
        .name(i.name)
        .code(i.code)
        .typeFullName(Defines.Any)
        .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
        .methodFullName(XDefines.DynamicCallUnknownFullName)
        .argumentName(i.argumentName)
        .argumentIndex(i.argumentIndex)
        .lineNumber(i.lineNumber)
        .columnNumber(i.columnNumber)
        .order(i.order)
      // Persist node in-place
      diffGraph.addNode(call)
      i.outE.filterNot(_.label == EdgeTypes.REF).foreach(e => diffGraph.addEdge(call, e.inNode, e.label))
      i.inE.foreach(e => diffGraph.addEdge(e.outNode, call, e.label))
      // Remove identifiers
      i.outE(EdgeTypes.REF).foreach(removedEdge.add)
      removedNodeMap.addOne(i.id(), i)
    }
    // Finally, remove local
    removedNodeMap.addOne(node.id(), node)
  }

}
