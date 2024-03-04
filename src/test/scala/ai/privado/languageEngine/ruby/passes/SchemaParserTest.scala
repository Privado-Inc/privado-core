package ai.privado.languageEngine.ruby.passes

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.RubyTestBase.*
import ai.privado.languageEngine.ruby.passes.SchemaParser
import ai.privado.semantic.Language.*
import better.files.File
import io.joern.rubysrc2cpg.deprecated.passes.RubyTypeHintCallLinker
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.passes.frontend.{LocalKey, SBKey, SymbolTable}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.model.SourceCodeModel

class SchemaParserTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "Schema parser" should {
    val (cpg, config) = code(
      List(
        SourceCodeModel(
          """
            |ActiveRecord::Schema[7.0].define(version: 2023_09_22_164903) do
            |  create_table "my_mappings", force: :cascade do |t|
            |    t.bigint "company_id", null: false
            |    t.bigint "center_id"
            |    t.bigint "target_id", null: false
            |  end
            |end
            |
            |""".stripMargin,
          "schema.rb"
        )
      )
    )
    new SchemaParser(cpg, config.inputPath, RuleCache()).createAndApply()

    "be able to create sql nodes" in {
      val table = cpg.sqlTable.l
      table.size shouldBe (1)
      table.name.l shouldBe List("my_mappings")
      table.lineNumber.l shouldBe List(3)

      val columns = cpg.sqlColumn.l
      columns.size shouldBe (3)
      columns.name.l shouldBe List("company_id", "center_id", "target_id")
      columns.lineNumber.l shouldBe List(4, 5, 6)
    }
  }

}
