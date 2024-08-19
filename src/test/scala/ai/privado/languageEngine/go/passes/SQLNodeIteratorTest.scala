package ai.privado.languageEngine.go.passes

import ai.privado.languageEngine.go.GoTestBase
import ai.privado.model.Constants
import ai.privado.semantic.language.*
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewSqlColumnNode}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*

// Pass will add a column node in cpg without edge to table node
class SQLColumnNodePass(cpg: Cpg) extends PrivadoParallelCpgPass[Integer](cpg) {

  override def generateParts(): Array[Integer] = List[Integer](1).toArray

  override def runOnPart(builder: DiffGraphBuilder, element: Integer): Unit = {
    val columnNode = NewSqlColumnNode()
      .name("test")
      .code("test")
      .lineNumber(Some(Integer.valueOf(-1)))
      .columnNumber(Some(Integer.valueOf(-1)))
      .order(1)
    builder.addEdge(columnNode, NewFile().name(Constants.Unknown), EdgeTypes.SOURCE_FILE)
  }
}

class SQLNodeIteratorTest extends GoTestBase {

  "Check and iterate over column node" should {
    val (cpg, _) = code("".stripMargin)
    new SQLColumnNodePass(cpg).createAndApply()

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 1
      val List(testNode) = cpg.sqlColumn.l
      testNode.code shouldBe "test"
      testNode.lineNumber shouldBe Some(-1)
      testNode.columnNumber shouldBe Some(-1)
    }

    "check iteration over column node" in {
      val sqlTable = cpg.sqlColumn.head.sqlTable
      assert(sqlTable.isEmpty)
    }
  }
}
