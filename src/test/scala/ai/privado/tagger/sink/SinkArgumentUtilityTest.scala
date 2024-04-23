package ai.privado.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.javascript.tagger.sink.RegularSinkTagger
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, FilterProperty, Language, NodeType, RuleInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import ai.privado.tagger.sink.SinkArgumentUtility
import better.files.File
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}

class SinkArgumentUtilityTest extends AnyFlatSpec with Matchers {

  "serializedArgumentString" should "serialize a list of tuples into a string" in {
    // Arrange
    val originalList = List(("key1", "value1"), ("key2", "value2"))

    // Act
    val serializedString = SinkArgumentUtility.serializedArgumentString(originalList)

    // Assert
    serializedString should not be empty
  }

  "deserializedArgumentString" should "deserialize a string into a map of strings" in {
    // Arrange
    val originalList     = List(("key1", "value1"), ("key2", "value2"))
    val serializedString = SinkArgumentUtility.serializedArgumentString(originalList)

    // Act
    val deserializedMap = SinkArgumentUtility.deserializedArgumentString(serializedString)

    // Assert
    deserializedMap should contain allOf ("key1" -> "value1", "key2" -> "value2")
  }

  it should "return an empty map for an empty string" in {
    // Arrange
    val serializedString = ""

    // Act
    val deserializedMap = SinkArgumentUtility.deserializedArgumentString(serializedString)

    // Assert
    deserializedMap shouldBe empty
  }

  /** Below test-case covers below conditions:
    *   1. Simple literal arguments 2. Nested arguments 3. Payload Method 4. lodash pick method 5. Block Node (part of
    *      nested structure) 6. Spread Node
    */
  "For googleTagManagerPixelRuleId verify the arguments list" should "tag the call node as sink by matching rule with cpg.code" in {
    val cpg = code("""
        |import { pick } from 'lodash';
        |const googlePayloads = {
        |    logProductImpression: {
        |        event: 'productImpressions',
        |        payload: ({ product, location, preprocessedData = {} }: LogProductImpressionArgs) => {
        |            return {
        |                ...pick(product, ['id', 'name', 'ext_id', 'ic_product_id']),
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
        |    nestedKey: {
        |        nested1: "N1",
        |        nestedDeep: {
        |            nestedDeep1: "N1.11"
        |        }
        |    },
        |    latitude: obj.latitude,
        |    testPayload: googlePayloads.logProductImpression.payload({}),
        |    ...products
        |});
        |""".stripMargin)
    val expectedArgumentsMap = Map(
      "nestedKey.nested1"                      -> "\"N1\"",
      "latitude"                               -> "obj.latitude",
      ".payload.ecommerce.impressions.[].list" -> "location",
      ".payload.ecommerce.impressions.[].category" -> "product.categories?.length > 0 ? product.categories[product.categories?.length - 1].name : \"\"",
      ".payload.\"id\""                         -> "product.\"id\"",
      ".payload.ecommerce.impressions.[].price" -> "product.sale_price || product.base_price",
      "nestedKey.nestedDeep.nestedDeep1"        -> "\"N1.11\"",
      ".payload.\"ext_id\""                     -> "product.\"ext_id\"",
      "event"                                   -> "googlePayloads.logProductImpression.event",
      ".payload.ecommerce.impressions."         -> "__ecma.Array.factory()",
      ".payload.event"                          -> "\"productImpressions\"",
      "products"                                -> "ANY",
      "testPayload"                             -> "googlePayloads.logProductImpression.payload({})",
      ".payload.pick.lodash"                    -> "lodash",
      ".payload.\"ic_product_id\""              -> "product.\"ic_product_id\"",
      ".payload.\"name\""                       -> "product.\"name\""
    )
    val gaSinkNode = cpg.call.name("push").head
    gaSinkNode.tag.nameExact(Constants.id).value.headOption shouldBe Some(Constants.googleTagManagerPixelRuleId)
    val encodedArgumentsList = gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{}")
    val argumentsList        = SinkArgumentUtility.deserializedArgumentString(encodedArgumentsList)

    // Iterate over each key-value pair in the argumentsMap
    expectedArgumentsMap.foreach { case (key, value) =>
      // Retrieve the corresponding value from the argumentsList
      val argumentsListValue = argumentsList.getOrElse(key, "")
      assert(argumentsListValue == value, s"Expected value '$value' for key '$key', but got '$argumentsListValue'")
    }
  }

  "For googleTagManagerPixelRuleId verify the arguments list" should "check for no arguments passed" in {
    val cpg = code("""
        |globalThis.dataLayer.push();
        |""".stripMargin)
    val gaSinkNode = cpg.call.name("push").head
    gaSinkNode.tag.nameExact(Constants.id).value.headOption shouldBe Some(Constants.googleTagManagerPixelRuleId)
    val encodedArgumentsList = gaSinkNode.tag.nameExact(Constants.arguments).value.headOption.getOrElse("{}")
    val argumentsList        = SinkArgumentUtility.deserializedArgumentString(encodedArgumentsList)
    argumentsList.isEmpty shouldBe true
  }

  "For other sinks verify" should "arguments tag should not be added" in {
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
  new RegularSinkTagger(cpg, ruleCache).createAndApply()
  cpg
}
