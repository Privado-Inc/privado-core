package ai.privado.inputprocessor

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*
import ai.privado.semantic.language.*

class DynamicRuleMergerTest extends JavaFrontendTestSuite, DynamicRuleMerger {

  private val existingRule = List(
    RuleInfo(
      "ThirdParties.SDK.AmazonS3",
      "Amazon S3",
      "Third Parties",
      FilterProperty.METHOD_FULL_NAME,
      Array("amazon.com"),
      List(".*aws.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.JAVA,
      Array()
    )
  )

  "Test merging logic when both domain and name are same" should {

    val dynamicRule = List(
      RuleInfo(
        "ThirdParties.SDK.AmazonS3",
        "Amazon S3",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("amazon.com"),
        List(".*(software.amazon.awssdk.services.s3).*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "",
        Language.JAVA,
        Array()
      )
    )

    val finalSinkRule = mergeDynamicRuleSinkForDependencyDiscovery(dynamicRule, existingRule)
    val configAndRule = ConfigAndRules(sinks = finalSinkRule)
    val ruleCache     = RuleCache().setRule(configAndRule)

    val cpg = code("""
        |import software.amazon.awssdk.services.s3.S3Client;
        |
        |public class AWS {
        |   public static void main(String[] args) {
        |     String firstName = "anything";
        |     S3Client s3client = S3Client.builder.build();
        |
        |     s3client.put(firstName);
        |   }
        |}
        |""".stripMargin)
      .withRuleCache(ruleCache)

    "correct sink detection" in {
      val sink = cpg.call("put").l
      sink.tag.nameExact(Constants.id).value.l shouldBe List("ThirdParties.SDK.AmazonS3")
    }

    "check ruleCache info" in {
      val sinkRule = ruleCache.getRule.sinks.filter(_.id.contains("ThirdParties"))
      sinkRule.size shouldBe 1
      sinkRule.headOption.get.domains.toList shouldBe List("amazon.com")
      sinkRule.headOption.get.patterns shouldBe List(".*aws.*", ".*(software.amazon.awssdk.services.s3).*")
    }
  }

  "Merging logic when domains are same but name different" should {

    val dynamicRule = List(
      RuleInfo(
        "ThirdParties.SDK.AmazonS3",
        "AWS S3",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("amazon.com"),
        List(".*(software.amazon.awssdk.services.s3).*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "",
        Language.JAVA,
        Array()
      )
    )

    val finalSinkRule = mergeDynamicRuleSinkForDependencyDiscovery(dynamicRule, existingRule)
    val configAndRule = ConfigAndRules(sinks = finalSinkRule)
    val ruleCache     = RuleCache().setRule(configAndRule)

    val cpg = code("""
        |import software.amazon.awssdk.services.s3.S3Client;
        |
        |public class AWS {
        |   public static void main(String[] args) {
        |     String firstName = "anything";
        |     S3Client s3client = S3Client.builder.build();
        |
        |     s3client.put(firstName);
        |   }
        |}
        |""".stripMargin)
      .withRuleCache(ruleCache)

    "correct sink detection" in {
      val sink = cpg.call("put").l
      sink.tag.nameExact(Constants.id).value.l shouldBe List("ThirdParties.SDK.AmazonS3")
    }

    "check ruleCache info" in {
      val sinkRule = ruleCache.getRule.sinks.filter(_.id.contains("ThirdParties"))
      sinkRule.size shouldBe 1
      sinkRule.headOption.get.name shouldBe "Amazon S3"
      sinkRule.headOption.get.domains.toList shouldBe List("amazon.com")
      sinkRule.headOption.get.patterns shouldBe List(".*aws.*", ".*(software.amazon.awssdk.services.s3).*")
    }
  }

  "Merging logic when both domains and name are different" should {

    val dynamicRule = List(
      RuleInfo(
        "ThirdParties.SDK.AmazonS3",
        "AWS S3",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("aws.amazon.com"),
        List(".*(software.amazon.awssdk.services.s3).*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "",
        Language.JAVA,
        Array()
      )
    )

    val finalSinkRule = mergeDynamicRuleSinkForDependencyDiscovery(dynamicRule, existingRule)
    val configAndRule = ConfigAndRules(sinks = finalSinkRule)
    val ruleCache     = RuleCache().setRule(configAndRule)

    val cpg = code("""
        |import software.amazon.awssdk.services.s3.S3Client;
        |
        |public class AWS {
        |   public static void main(String[] args) {
        |     String firstName = "anything";
        |     S3Client s3client = S3Client.builder.build();
        |
        |     s3client.put(firstName);
        |   }
        |}
        |""".stripMargin)
      .withRuleCache(ruleCache)

    "correct sink tagging" in {
      val sink = cpg.call("put").l
      sink.tag.nameExact(Constants.id).value.l shouldBe List("ThirdParties.SDK.AmazonS3")
    }

    "check ruleCache info" in {
      val sinkRule = ruleCache.getRule.sinks.filter(_.id.contains("ThirdParties"))
      sinkRule.size shouldBe 2
    }
  }

  "No merging when have filterProperty as code" should {
    val existingCodeRule = List(
      RuleInfo(
        "ThirdParties.SDK.AmazonS3",
        "Amazon S3",
        "Third Parties",
        FilterProperty.CODE,
        Array("amazon.com"),
        List(".*(aws).*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "",
        Language.JAVA,
        Array()
      )
    )

    val dynamicFilterPropertyRule = List(
      RuleInfo(
        "ThirdParties.SDK.AmazonS3",
        "Amazon S3",
        "Third Parties",
        FilterProperty.METHOD_FULL_NAME,
        Array("amazon.com"),
        List(".*(software.amazon.awssdk.services.s3).*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "",
        Language.JAVA,
        Array()
      )
    )

    val finalSinkRule = mergeDynamicRuleSinkForDependencyDiscovery(dynamicFilterPropertyRule, existingCodeRule)
    val configAndRule = ConfigAndRules(sinks = finalSinkRule)
    val ruleCache     = RuleCache().setRule(configAndRule)

    val cpg = code("""
        |import software.amazon.awssdk.services.s3.S3Client;
        |
        |public class AWS {
        |   public static void main(String[] args) {
        |     String firstName = "anything";
        |     S3Client s3client = S3Client.builder.build();
        |
        |     s3client.put(firstName);
        |   }
        |}
        |""".stripMargin)
      .withRuleCache(ruleCache)

    "correct sink detection" in {
      val sink = cpg.call("put").l
      sink.tag.nameExact(Constants.id).value.l shouldBe List("ThirdParties.SDK.AmazonS3")
    }

    "check ruleCache info" in {
      val sinkRule = ruleCache.getRule.sinks.filter(_.id.contains("ThirdParties"))
      sinkRule.size shouldBe 2
    }
  }
}
