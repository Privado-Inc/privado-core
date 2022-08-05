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
