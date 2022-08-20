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

package ai.privado.entrypoint

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{Credentials, NewLiteral, NewMethod, Literal}
import io.shiftleft.passes.SimpleCpgPass
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.testing.MockCpg
import better.files.{File => F}
import overflowdb.traversal._

class MyTest extends AnyWordSpec with Matchers {

  "Foo" should {

    "description1" in {
      val cpg = MockCpg()
        .withMethod("foo")
        .withCallInMethod("foo", "bar")
        .withCustom { case (diffGraph, cpg) =>
          val literal = NewLiteral().code("abc")
          diffGraph.addNode(literal)
          val block = cpg.method.block.head
          diffGraph.addEdge(block, literal, EdgeTypes.AST)
        }
        .cpg
      cpg.method.block.astChildren.isLiteral.foreach(println)
    }

    "description2" in {
      F.usingTemporaryFile("standalone") { file =>
        println(file)
      }
    }

    "MyPass" should {
      "create a credential node" in {
        val cpg = MockCpg()
          .withMethod("abc")
          .withCallInMethod("abc", "foo")
          .withLiteralArgument("foo", "password")
          .cpg

        new MyPass(cpg).createAndApply()
        new MyCredPass(cpg).createAndApply()
        cpg.graph.V.foreach(println)
        val List(x: Credentials) = cpg.graph.V.label("CREDENTIALS").cast[Credentials].l
        val List(l: Literal)     = x.in(EdgeTypes.IS_CREDENTIAL).toList
        l.code shouldBe "password"
      }
    }
  }

}
