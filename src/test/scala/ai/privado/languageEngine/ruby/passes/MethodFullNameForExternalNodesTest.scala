package ai.privado.languageEngine.ruby.passes

import ai.privado.cache.RuleCache
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.shiftleft.semanticcpg.language.*
import ai.privado.utility.Utilities.resolver
import ai.privado.languageEngine.ruby.RubyTestBase.*
import ai.privado.languageEngine.ruby.passes.download.DownloadDependenciesPass
import io.joern.rubysrc2cpg.RubySrc2Cpg
import io.joern.rubysrc2cpg.deprecated.utils.PackageTable
import ai.privado.model.SourceCodeModel

class MethodFullNameForExternalNodesTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "method fullname for external nodes accessed via scopeResolution" should {

    "be resolved" in {
      val (cpg, config) = code(
        List(
          SourceCodeModel(
            """
          |class MyClass
          |   def foo
          |     zendesk_user = ZendeskAPI::User.create_or_update!(
          |          external_id: user.strong_id,
          |          email: user.email,
          |          name: user.name,
          |          user_fields: user_fields(user),
          |        )
          |   end
          |end
          |""".stripMargin,
            "demo.rb"
          ),
          SourceCodeModel(
            """
          |source 'https://rubygems.org'
          |gem 'zendesk_api'
          |""".stripMargin,
            "Gemfile"
          )
        )
      )

      val packageTable =
        new DownloadDependenciesPass(new PackageTable(), config.inputPath, RuleCache()).createAndApply()
      new RubyExternalTypesPass(cpg, packageTable).createAndApply()

      cpg.call("create_or_update!").dynamicTypeHintFullName.l shouldBe List(
        "zendesk_api::program.ZendeskAPI.User.create_or_update!"
      )

    }
  }
}
