package ai.privado.audit

import ai.privado.audit.DEDSourceDiscovery
import ai.privado.languageEngine.ruby.RubyTestBase.code
import ai.privado.model.{Language, SourceCodeModel}

import scala.util.Try
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class RubyDEDSourceDiscoveryTest extends AnyWordSpec with Matchers {
  "Check ded source discovery results" in {
    val (cpg, _) = code(
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
    val dedSources = DEDSourceDiscovery.processDEDSourceDiscovery(Try(cpg), Language.RUBY).drop(1)
    val expectedResult = List(
      "PersonalInfo,Identifier,ANY",
      "prepare,Identifier,ANY",
      "EmployerOfRecordType,Identifier,ANY",
      "employment_attributes,Identifier,ANY",
      "personal_info,Method Parameter,ANY",
      "MyMutation,Identifier,ANY",
      "required,Identifier,ANY",
      "Employment,Identifier,ANY",
      "prepare,Identifier,ANY",
      "required,Identifier,ANY"
    )
    val res: List[String] = dedSources.map(i => {
      s"${i(3)},${i(12)},${i(0)}"
    })
    res shouldBe expectedResult
  }

}
