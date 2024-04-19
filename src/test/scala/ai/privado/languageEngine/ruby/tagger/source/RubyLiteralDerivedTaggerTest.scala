package ai.privado.languageEngine.ruby.tagger.source

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
import ai.privado.model.SourceCodeModel
import ai.privado.rule.RuleInfoTestData

class RubyLiteralDerivedTaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val ruleCache = new RuleCache()
  ruleCache.withRule(
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

  "Ruby literal derived tagger" should {
    val (cpg, config) = code(
      List(
        SourceCodeModel(
          """
        |
        |module Types
        |  module Employment
        |    module PersonalInfo
        |      class AttributesInputType < Types::BaseInputObject
        |
        |        argument :first_name, String, required: false, prepare: :strip
        |
        |      end
        |    end
        |  end
        |end
        |""".stripMargin,
          "sample1.rb"
        ),
        SourceCodeModel(
          """
        |
        |module Mutations
        |  class MyMutation < Mutations::BaseMutation
        |    class CreateError < StandardError; end
        |
        |    argument :employer_of_record_id, String, required: false, loads: Types::EmployerOfRecordType, prepare: :strip
        |    argument :personal_info, Types::Employment::PersonalInfo::AttributesInputType, required: false
        |
        |
        |    def resolve(personal_info: {}, **arguments)
        |      perform(
        |        personal_info: personal_info.to_h,
        |        employment_attributes: arguments.to_h,
        |      )
        |    end
        |  end
        |end
        |""".stripMargin,
          "sample2.rb"
        )
      )
    )

    "tag derived source via literal" in {
      new RubyLiteralTagger(cpg, ruleCache).createAndApply()
      new RubyLiteralDerivedTagger(cpg, ruleCache).createAndApply()

      cpg
        .literal(":personal_info")
        .lineNumber(8)
        .tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .size shouldBe 1

      val derivedIdentifiers = cpg.identifier("personal_info").lineNumber(13).l
      derivedIdentifiers.size shouldBe (2)
      derivedIdentifiers.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .size shouldBe 2
    }
  }
}
