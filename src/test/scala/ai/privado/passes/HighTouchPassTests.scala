package ai.privado.passes

import ai.privado.languageEngine.default.NodeStarters
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import ai.privado.rule.RuleInfoTestData
import ai.privado.semantic.Language.NodeStarterForSqlQueryNode
import ai.privado.testfixtures.DefaultFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class HighTouchPassTests extends DefaultFrontendTestSuite {

  "hightouch pass" should {
    "tag sources and sinks in yaml files" in {
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
        .withRuleCache(RuleInfoTestData.ruleCache)

      val src  = cpg.sqlColumn.where(_.tag.nameExact(InternalTag.VARIABLE_REGEX_LITERAL.toString)).l
      val sink = cpg.highTouchSink.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l
      src.size shouldBe 2
      src.lineNumber.l shouldBe List(8, 8)
      sink.size shouldBe 1
      sink.lineNumber.l shouldBe List(3)
    }
  }
}
