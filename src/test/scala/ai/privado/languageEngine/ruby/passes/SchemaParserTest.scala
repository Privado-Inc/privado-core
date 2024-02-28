package ai.privado.languageEngine.ruby.passes

import ai.privado.RuleInfoTestData
import ai.privado.cache.RuleCache
import ai.privado.languageEngine.ruby.RubyTestBase.*
import ai.privado.languageEngine.ruby.passes.SchemaParser
import ai.privado.languageEngine.ruby.tagger.source.{IdentifierDerivedTagger, RubyLiteralTagger}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, RuleInfo}
import ai.privado.semantic.Language.*
import ai.privado.tagger.source.SqlQueryTagger
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

class SchemaParserTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val ruleCache = new RuleCache()
  ruleCache.setRule(
    ConfigAndRules(
      sources = RuleInfoTestData.sourceRule,
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      List()
    )
  )

  "Schema parser" should {
    val (cpg, config) = code(
      List(
        SourceCodeModel(
          """
            |ActiveRecord::Schema[7.0].define(version: 2023_09_22_164903) do
            |  create_table "my_mappings", force: :cascade do |t|
            |    t.bigint "firstName", null: false
            |    t.bigint "center_id"
            |    t.bigint "target_id", null: false
            |  end
            |end
            |
            |""".stripMargin,
          "schema.rb"
        ),
        SourceCodeModel(
          """
            |
            |module Mutations
            |  class Create < Mutations::BaseMutation
            |    class MappingCreateError < StandardError; end
            |
            |    argument :firstName, String, required: false, prepare: :strip
            |    field :my_mappings, Types::Mapping, null: false
            |    field :myMapping, Types::Mapping, null: true
            |    field :myMappings, Types::Mapping, null: true
            |
            |    def resolve(my_mappings:)
            |      perform(my_mappings)
            |    end
            |  end
            |end
            |
            |""".stripMargin,
          "sample.rb"
        )
      )
    )
    new SchemaParser(cpg, config.inputPath, RuleCache()).createAndApply()
    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    "be able to create sql nodes" in {
      val table = cpg.sqlTable.l
      table.size shouldBe (1)
      table.name.l shouldBe List("my_mappings")
      table.lineNumber.l shouldBe List(3)

      val columns = cpg.sqlColumn.l
      columns.size shouldBe (3)
      columns.name.l shouldBe List("firstName", "center_id", "target_id")
      columns.lineNumber.l shouldBe List(4, 5, 6)
    }

    "be able to tag derived sources" in {
      new IdentifierDerivedTagger(cpg, ruleCache).createAndApply()
      cpg
        .identifier("my_mappings")
        .lineNumber(13)
        .tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .size shouldBe 1
      cpg.literal
        .code(":my_mappings")
        .lineNumber(8)
        .tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .size shouldBe 1
    }

    "be able to tag original sources" in {
      new RubyLiteralTagger(cpg, ruleCache).createAndApply()
      cpg.literal
        .code(":firstName")
        .lineNumber(7)
        .tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.SOURCES.name)
        .size shouldBe 1
    }
  }

}
