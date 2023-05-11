/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.semantic

import ai.privado.model.Semantic
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, Method}
import io.shiftleft.semanticcpg.language._
import ai.privado.model.Language.UNKNOWN
import overflowdb.traversal.Traversal

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

trait SemanticGenerator {

  implicit val resolver: ICallResolver = NoResolve

  /** Utility to get the default semantics for dataflow queries
    *
    * @return
    */
  def getDefaultSemantics: Semantics = {
    DefaultSemantics()
  }

  /** Generate semantics for tainting passed argument based on the number of parameter in method signature
    *
    * @param method
    *   or call node \- complete signature of method
    * @return
    *   \- semantic string
    */
  def generateSemanticForTaint(methodNode: AstNode, toTaint: Int = -2, extraParameter: Int = 0) = {
    val (parameterSize, fullName) = {
      methodNode match {
        case method: Method => (method.parameter.size + extraParameter, method.fullName)
        case call: Call     => (call.argument.size + extraParameter, call.methodFullName)
        case _              => (0, "NA")
      }
    }
    val parameterSemantic = mutable.HashSet[String]()
    for (i <- 0 until parameterSize) {
      if (toTaint != -2)
        parameterSemantic.add(s"$i->$toTaint")
      parameterSemantic.add(s"$i->$i")
    }
    Semantic(fullName, parameterSemantic.toList.sorted.mkString(" ").trim, "", UNKNOWN, Array())
  }

  /** Generate Semantic string based on input Semantic
    *
    * @param semantic
    *   \- semantic object containing semantic information
    * @return
    */
  def generateSemantic(semantic: Semantic): Option[String] = {
    if (semantic.signature.nonEmpty) {
      val generatedSemantic = "\"" + semantic.signature.trim + "\" " + semantic.flow
      Some(generatedSemantic.trim)
    } else
      None
  }

  /** Takes sequence of semantic as input and returns the unique semantic by signature which have the longest flow
    *
    * ex - If we have 2 semantics with the same signature, we would want the maximum flow one
    *   1. "logging.py:<module>.getLogger.<returnValue>.info" 0->-1 0->0 1->-1 1->1 2->-1 2->2 3->-1 3->3 4->-1 4->4
    *      5->-1 5->5
    *
    * 2. "logging.py:<module>.getLogger.<returnValue>.info" 0->-1 0->0 1->-1 1->1 2->-1 2->2
    *
    * We want the output to be 1st one as it has the longer flow
    * @param semantics
    * @return
    */
  def getMaximumFlowSemantic(semantics: Traversal[Semantic]): Seq[String] = {
    semantics.l.par
      .groupBy(_.signature)
      .map(_._2.sortBy(_.flow).last)
      .flatMap(generateSemantic)
      .sorted
  }
}
