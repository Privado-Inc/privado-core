package ai.privado.languageEngine.ruby.tagger.schema

import ai.privado.cache.{DatabaseDetailsCache, PropertyFilterCache, RuleCache}
import ai.privado.languageEngine.ruby.RubyTestBase.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.model.{
  ConfigAndRules,
  DatabaseColumn,
  DatabaseDetails,
  DatabaseSchema,
  DatabaseTable,
  Language,
  SourceCodeModel
}
import ai.privado.rule.RuleInfoTestData
import ai.privado.utility.PropertyParserPass

class RubyMongoSchemaMapperTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val ruleCache = new RuleCache()
  ruleCache.setRule(ConfigAndRules(sources = RuleInfoTestData.sourceRule))

  "Ruby mongo schema mapper" should {
    val (cpg, cpgConfig) = code(
      List(
        SourceCodeModel(
          """
        |aliases:
        |  - &base_mongoid_specific_options
        |    options:
        |      include_root_in_json: false
        |      raise_not_found_error: false
        |production:
        |  <<: *base_mongoid_specific_options
        |  clients:
        |    default:
        |      database: default_production
        |    noncustomer_default:
        |      database: noncustomer_production
        |    people:
        |      database: people_production
        |""".stripMargin,
          "config/mongoid.yml"
        ),
        SourceCodeModel(
          """
        |class User
        |  include Mongoid::Document
        |  include Mongoid::Timestamps
        |
        |  belongs_to :team
        |  belongs_to :permission_set
        |
        |  field :last_logged_in, type: DateTime
        |  field :last_logged_out, type: DateTime
        |  field :ip_address, type: String
        |  field :session_ids, type: Array, default: []
        |  field :support_emails_sent_at, type: Hash, default: {}
        |  field :last_name, type: String
        |  field :first_name, type: String
        |  field :title, type: String
        |end
        |""".stripMargin,
          "User.rb"
        )
      )
    )

    new PropertyParserPass(cpg, cpgConfig.inputPath, ruleCache, Language.RUBY, PropertyFilterCache())
      .createAndApply()
    new RubyMongoSchemaTagger(cpg, ruleCache).createAndApply()
    new RubyMongoSchemaMapper(cpg, ruleCache).createAndApply()
    val databaseDetailsCache = DatabaseDetailsCache.getAllDatabaseDetails

    "detect all databases in yaml" in {
      databaseDetailsCache.size shouldBe 3
      databaseDetailsCache
        .map(_.dbName) shouldBe List("noncustomer_production", "people_production", "default_production")
    }

    "detect schema for User" in {
      val defaultDB = databaseDetailsCache.filter(_.dbName == "default_production").head
      defaultDB shouldBe DatabaseDetails(
        "default_production",
        "",
        "",
        "",
        "",
        Some(
          DatabaseSchema(
            "",
            "production",
            "",
            List(
              DatabaseTable(
                "users",
                "",
                List(
                  DatabaseColumn("last_logged_in", "", "DateTime", ""),
                  DatabaseColumn("last_logged_out", "", "DateTime", ""),
                  DatabaseColumn("ip_address", "", "String", ""),
                  DatabaseColumn("session_ids", "", "Array", ""),
                  DatabaseColumn("support_emails_sent_at", "", "Hash", ""),
                  DatabaseColumn("last_name", "", "String", ""),
                  DatabaseColumn("first_name", "", "String", ""),
                  DatabaseColumn("title", "", "String", "")
                )
              )
            )
          )
        )
      )
    }

  }

}
