package ai.privado.passes

import ai.privado.languageEngine.default.NodeStarters
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import ai.privado.rule.RuleInfoTestData
import ai.privado.semantic.language.Language.NodeStarterForSqlQueryNode
import ai.privado.testfixtures.DefaultFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class HighTouchPassTests extends DefaultFrontendTestSuite {

  "hightouch pass" should {

    val cpg = code(
      """
        |sources:
        |  snowflake:
        |    name: Snowflake - EXT_HIGHTOUCH
        |    type: snowflake
        |destinations:
        |  the-trade-desk:
        |    name: The Trade Desk
        |    type: tradedesk
        |  the-google-ads:
        |    name: Google Ads
        |    type: google
        |""".stripMargin,
      "manifest.yaml"
    )
      .moreCode(
        """
          |name: 'Some Model '
          |source: snowflake-service-hightouch-outbound
          |description: >-
          | Some description
          |type: raw_sql
          |rawSql: >
          |  SELECT salary, email FROM user;
          |primaryKey: salary
          |""".stripMargin,
        "some-unique-id.yaml"
      )
      .moreCode(
        """
          |model: some-unique-id
          |destination: the-trade-desk
          |description: >-
          |  Some description
          |config:
          |  key1: key1
          |  key2: object
          |  nested:
          |    - key1: key1
          |      key2: key2
          |schedulePaused: false
          |""".stripMargin,
        "sync.yaml"
      )
      .moreCode(
        """
          |model: some-unique-id
          |destination: the-google-ads
          |description: >-
          |  Some description
          |config:
          |  key1: key1
          |  key2: object
          |  nested:
          |    - key1: key1
          |      key2: key2
          |schedulePaused: true
          |""".stripMargin,
        "sync2.yaml"
      )
      .withRuleCache(RuleInfoTestData.ruleCache)

    "tag sources with correct attributes in model files" in {
      val src = cpg.sqlColumn.where(_.tag.nameExact(InternalTag.VARIABLE_REGEX_LITERAL.toString)).l
      src.size shouldBe 2
      src.lineNumber.l shouldBe List(8, 8)
    }

    "tag sinks with correct attributes in sync files" in {
      val sink = cpg.highTouchSink.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l
      sink.size shouldBe 1
      sink.lineNumber.l shouldBe List(3)
    }

    "should not tag sinks whose schedule is paused" in {
      val googleAdsSink = cpg.highTouchSink.filter(_.schedulePaused)
      cpg.highTouchSink
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
        .l
        .contains(googleAdsSink) shouldBe false
    }
  }
}
