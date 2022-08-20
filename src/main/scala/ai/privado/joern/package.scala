/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

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
