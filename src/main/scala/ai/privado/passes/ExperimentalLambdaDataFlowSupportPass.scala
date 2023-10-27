package ai.privado.passes

import io.joern.dataflowengineoss.passes.reachingdef.*
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.joern.dataflowengineoss.{DefaultSemantics, globalFromLiteral, identifierToFirstUsages}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, PropertyNames}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

/** Enables DDG edges from variables outside of a closure to their references inside.
  */
class ExperimentalLambdaDataFlowSupportPass(cpg: Cpg, maxNumberOfDefinitions: Int = 4000)(implicit
  s: Semantics = DefaultSemantics()
) extends ForkJoinParallelCpgPass[Method](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[Method] = cpg.method.toArray

  override def runOnPart(builder: DiffGraphBuilder, method: Method): Unit = {
    implicit val diffGraph: DiffGraphBuilder = builder
    val problem                              = ReachingDefProblem.create(method)
    if (shouldBailOut(method, problem)) {
      logger.warn("Skipping.")
    } else {
      addEdgesToCapturedIdentifiersAndParameters(method)
    }
  }

  private def addEdgesToCapturedIdentifiersAndParameters(method: Method)(implicit dstGraph: DiffGraphBuilder): Unit = {
    val identifierDestPairs =
      method._identifierViaContainsOut.flatMap { identifier =>
        val firstAndLastUsageByMethod = identifierToFirstUsages(identifier).groupBy(_.method)
        firstAndLastUsageByMethod.values
          .filter(_.nonEmpty)
          .map(x => (x.head, x.last))
          .flatMap { case (firstUsage, lastUsage) =>
            (identifier.lineNumber, firstUsage.lineNumber, lastUsage.lineNumber) match {
              case (Some(iNo), Some(fNo), _) if iNo <= fNo => Some(identifier, firstUsage)
              case (Some(iNo), _, Some(lNo)) if iNo >= lNo => Some(lastUsage, identifier)
              case _                                       => None
            }
          }
      }.distinct

    identifierDestPairs
      .foreach { case (src, dst) =>
        addEdge(src, dst, nodeToEdgeLabel(src))
      }
    method.parameter.foreach { param =>
      param.capturedByMethodRef.referencedMethod.ast.isIdentifier.foreach { identifier =>
        addEdge(param, identifier, nodeToEdgeLabel(param))
      }
    }
    val globalIdentifiers =
      method.ast.isLiteral.flatMap(globalFromLiteral).collectAll[Identifier].l
    globalIdentifiers
      .foreach { global =>
        identifierToFirstUsages(global).map { identifier =>
          addEdge(global, identifier, nodeToEdgeLabel(global))
        }
      }
  }

  /** Before we start propagating definitions in the graph, which is the bulk of the work, we check how many definitions
    * were are dealing with in total. If a threshold is reached, we bail out instead, leaving reaching definitions
    * uncalculated for the method in question. Users can increase the threshold if desired.
    */
  private def shouldBailOut(method: Method, problem: DataFlowProblem[StoredNode, mutable.BitSet]): Boolean = {
    val transferFunction = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction]
    // For each node, the `gen` map contains the list of definitions it generates
    // We add up the sizes of these lists to obtain the total number of definitions
    val numberOfDefinitions = transferFunction.gen.foldLeft(0)(_ + _._2.size)
    logger.info("Number of definitions for {}: {}", method.fullName, numberOfDefinitions)
    if (numberOfDefinitions > maxNumberOfDefinitions) {
      logger.warn("{} has more than {} definitions", method.fullName, maxNumberOfDefinitions)
      true
    } else {
      false
    }
  }

  private def nodeToEdgeLabel(node: StoredNode): String = {
    node match {
      case n: MethodParameterIn => n.name
      case n: CfgNode           => n.code
      case _                    => ""
    }
  }

  private def edgeExists(fromNode: CfgNode, toNode: CfgNode, variable: String = ""): Boolean = {
    fromNode.outE(EdgeTypes.REACHING_DEF).exists { e =>
      e.inNode() == toNode && e.property(PropertyNames.VARIABLE) == variable
    }
  }

  private def addEdge(fromNode: StoredNode, toNode: StoredNode, variable: String = "")(implicit
    dstGraph: DiffGraphBuilder
  ): Unit = {
    if (!fromNode.isInstanceOf[Unknown] && !toNode.isInstanceOf[Unknown]) {
      (fromNode, toNode) match {
        case (parentNode: CfgNode, childNode: CfgNode)
            if !edgeExists(parentNode, childNode, variable) && EdgeValidator.isValidEdge(childNode, parentNode) =>
          dstGraph.addEdge(fromNode, toNode, EdgeTypes.REACHING_DEF, PropertyNames.VARIABLE, variable)
        case _ =>

      }
    }
  }

}
