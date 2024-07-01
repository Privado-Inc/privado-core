package ai.privado.tagger.sink

import ai.privado.cache.{DatabaseDetailsCache, RuleCache}
import ai.privado.languageEngine.javascript.tagger.sink.RegularSinkTagger
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, FilterProperty, Language, NodeType, RuleInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import ai.privado.tagger.sink.SinkArgumentUtility
import better.files.File
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}

class SinkArgumentUtilityTest extends AnyWordSpec with Matchers {

  "serializedArgumentString" should {
    "serialize a list of tuples into a string" in {
      // Arrange
      val originalList = List(("key1", "value1"), ("key2", "value2"))

      // Act
      val serializedString = SinkArgumentUtility.serializedArgumentString(originalList)

      // Assert
      serializedString should not be empty
    }
  }

  "deserializedArgumentString" should {
    "deserialize a string into a map of strings" in {
      // Arrange
      val originalList     = List(("key1", "value1"), ("key2", "value2"))
      val serializedString = SinkArgumentUtility.serializedArgumentString(originalList)

      // Act
      val deserializedMap = SinkArgumentUtility.deserializedArgumentString(serializedString)

      // Assert
      deserializedMap should contain allOf ("key1" -> "value1", "key2" -> "value2")
    }

    "return an empty map for an empty string" in {
      // Arrange
      val serializedString = ""

      // Act
      val deserializedMap = SinkArgumentUtility.deserializedArgumentString(serializedString)

      // Assert
      deserializedMap shouldBe empty
    }
  }

  /** Below test-case covers below conditions:
    *   1. Simple literal arguments 2. Nested arguments 3. Payload Method 4. lodash pick method 5. Block Node (part of
    *      nested structure) 6. Spread Node
    */
  "For googleTagManagerPixelRuleId verify the arguments list" should {
    "Check simple Identifier & Literal arguments" in {
      val cpg = code("""
                       |globalThis.dataLayer.push({
                       |    latitude: obj.latitude,
                       |    nested1: "N1"
                       |});
                       |""".stripMargin)
      val expectedArgumentsMap = Map("nested1" -> "\"N1\"", "latitude" -> "obj.latitude")

      val gaSinkNode = cpg.call.name("push").head
      gaSinkNode.tag.nameExact(Constants.id).value.headOption shouldBe Some(Constants.googleTagManagerPixelRuleId)
      val encodedArgumentsList = gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{}")
      val argumentsList        = SinkArgumentUtility.deserializedArgumentString(encodedArgumentsList)
      compareArgumentsList(expectedArgumentsMap, argumentsList)
    }

    "Check nested field arguments" in {
      val cpg = code("""
          |globalThis.dataLayer.push({
          |    nestedKey: {
          |        nested1: "N1",
          |        nestedDeep: {
          |            nestedDeep1: "N1.11"
          |        }
          |    }
          |});
          |""".stripMargin)
      val expectedArgumentsMap = Map("nestedKey.nested1" -> "\"N1\"", "nestedKey.nestedDeep.nestedDeep1" -> "\"N1.11\"")

      val gaSinkNode = cpg.call.name("push").head
      gaSinkNode.tag.nameExact(Constants.id).value.headOption shouldBe Some(Constants.googleTagManagerPixelRuleId)
      val encodedArgumentsList = gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{}")
      val argumentsList        = SinkArgumentUtility.deserializedArgumentString(encodedArgumentsList)
      compareArgumentsList(expectedArgumentsMap, argumentsList)
    }

    "Check pick call node arguments" in {
      val cpg = code("""
          |import { pick } from 'lodash';
          |globalThis.dataLayer.push({
          |    latitude: obj.latitude,
          |    ...pick(product, ['id', 'name', 'ext_id', 'ic_product_id'])
          |});
          |""".stripMargin)
      val expectedArgumentsMap = Map(
        "latitude"           -> "obj.latitude",
        ".\"id\""            -> "product.\"id\"",
        ".\"ext_id\""        -> "product.\"ext_id\"",
        "product"            -> "ANY",
        ".pick.lodash"       -> "",
        ".\"ic_product_id\"" -> "product.\"ic_product_id\"",
        ".\"name\""          -> "product.\"name\""
      )

      val gaSinkNode = cpg.call.name("push").head
      gaSinkNode.tag.nameExact(Constants.id).value.headOption shouldBe Some(Constants.googleTagManagerPixelRuleId)
      val encodedArgumentsList = gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{}")
      val argumentsList        = SinkArgumentUtility.deserializedArgumentString(encodedArgumentsList)
      compareArgumentsList(expectedArgumentsMap, argumentsList)
    }

    "Check payload call mapping with eventName & verify the arguments " in {
      val cpg = code("""
          |const googlePayloads = {
          |    logProductImpression: {
          |        event: 'productImpressions',
          |        payload: ({ product, location, preprocessedData = {} }: LogProductImpressionArgs) => {
          |            return {
          |                ecommerce: {
          |                    impressions: [
          |                        {
          |                            price: product.sale_price || product.base_price,
          |                            category:
          |                                product.categories?.length > 0
          |                                ? product.categories[product.categories?.length - 1]?.name
          |                                : '',
          |                            list: location,
          |                        },
          |                    ],
          |                },
          |                ...preprocessedData,
          |            }
          |        },
          |    }
          |};
          |globalThis.dataLayer.push({ /* <=== globalThis */
          |    event: googlePayloads.logProductImpression.event,
          |    testPayload: googlePayloads.logProductImpression.payload({})
          |});
          |""".stripMargin)
      val expectedArgumentsMap = Map(
        ".payload.ecommerce.impressions.[].list" -> "location",
        ".payload.ecommerce.impressions.[].category" -> "product.categories?.length > 0 ? product.categories[product.categories?.length - 1].name : \"\"",
        ".payload.ecommerce.impressions.[].price" -> "product.sale_price || product.base_price",
        "event"                                   -> "googlePayloads.logProductImpression.event",
        ".payload.ecommerce.impressions."         -> "__ecma.Array.factory()",
        "testPayload"                             -> "googlePayloads.logProductImpression.payload({})"
      )

      val gaSinkNode = cpg.call.name("push").head
      gaSinkNode.tag.nameExact(Constants.id).value.headOption shouldBe Some(Constants.googleTagManagerPixelRuleId)
      val encodedArgumentsList = gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{}")
      val argumentsList        = SinkArgumentUtility.deserializedArgumentString(encodedArgumentsList)
      compareArgumentsList(expectedArgumentsMap, argumentsList)
    }

    "Check spread operator call node arguments" ignore {
      val cpg = code("""
          |const products = {
          |        p1: {
          |            q1: "q1"
          |        },
          |        p2: "p2"
          |};
          |globalThis.dataLayer.push({ /* <=== globalThis */
          |    event: googlePayloads.logProductImpression.event,
          |    ...products
          |});
          |""".stripMargin)
      val expectedArgumentsMap =
        Map("p1.q1" -> "\"q1\"", "p2" -> "\"q2\"", "event" -> "googlePayloads.logProductImpression.event")

      val gaSinkNode = cpg.call.name("push").head
      gaSinkNode.tag.nameExact(Constants.id).value.headOption shouldBe Some(Constants.googleTagManagerPixelRuleId)
      val encodedArgumentsList = gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{}")
      val argumentsList        = SinkArgumentUtility.deserializedArgumentString(encodedArgumentsList)
      compareArgumentsList(expectedArgumentsMap, argumentsList)
    }
  }

  "For the googleTagManagerPixelRuleId, the verification of the arguments list" should {
    "ensure that no arguments are passed" in {
      val cpg = code("""
                       |globalThis.dataLayer.push();
                       |""".stripMargin)
      val gaSinkNode = cpg.call.name("push").head
      gaSinkNode.tag.nameExact(Constants.id).value.headOption shouldBe Some(Constants.googleTagManagerPixelRuleId)
      val encodedArgumentsList = gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{}")
      val argumentsList        = SinkArgumentUtility.deserializedArgumentString(encodedArgumentsList)
      argumentsList.isEmpty shouldBe true
    }
  }

  "For other sinks, the verification process" should {
    "ensure that the arguments tag is not added" in {
      val cpg = code("""
                       |const products = { "a": "a1", "b": "b1"  };
                       |pick(products, ["a", "b"]);
                       |""".stripMargin)
      val gaSinkNode = cpg.call.name("pick").head
      val encodedArgumentsList =
        gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{ \"empty\": true }")
      encodedArgumentsList shouldBe "{ \"empty\": true }"
    }
  }
}

val sinkRule = List(
  RuleInfo(
    Constants.segmentPixelRuleId,
    "Segment Pixel",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List(
      ".*(logSegmentEvent|trackSegment|pageViewSegment)",
      "(?i).*(analytics.track|trackCanonical|trackAnalyticsActions|useTracking.*track|tracking.*useTrack|tracking.*trackEvent).*"
    ),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SINKS,
    catLevelTwo = Constants.third_parties,
    Language.JAVASCRIPT,
    Array()
  ),
  RuleInfo(
    Constants.googleTagManagerPixelRuleId,
    "Google Tag Manager",
    "",
    FilterProperty.CODE,
    Array(),
    List("(?i)(.*[.])?(datalayer[.]push|gtag)[(].*"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SINKS,
    catLevelTwo = Constants.third_parties,
    Language.JAVASCRIPT,
    Array()
  )
)
def code(code: String): Cpg = {
  val inputDir = File.newTemporaryDirectory()
  (inputDir / "sample.js").write(code)
  val outputFile = File.newTemporaryFile()
  val rule: ConfigAndRules =
    ConfigAndRules(List(), sinkRule, List(), List(), List(), List(), List(), List(), List(), List())
  val ruleCache = new RuleCache()
  ruleCache.setRule(rule)
  val config = Config().withInputPath(inputDir.toString()).withOutputPath(outputFile.toString())
  val cpg    = new JsSrc2Cpg().createCpgWithAllOverlays(config).get
  new RegularSinkTagger(cpg, ruleCache, DatabaseDetailsCache()).createAndApply()
  cpg
}

def compareArgumentsList(expectedArgumentsMap: Map[String, String], argumentsList: Map[String, String]): Unit = {
  // Iterate over each key-value pair in the argumentsMap
  expectedArgumentsMap.foreach { case (key, value) =>
    // Retrieve the corresponding value from the argumentsList
    val argumentsListValue = argumentsList.getOrElse(key, "")
    assert(argumentsListValue == value, s"Expected value '$value' for key '$key', but got '$argumentsListValue'")
  }
}
