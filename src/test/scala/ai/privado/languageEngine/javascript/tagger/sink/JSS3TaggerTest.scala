package ai.privado.languageEngine.javascript.tagger.sink

import ai.privado.cache.{AppCache, RuleCache, S3DatabaseDetailsCache, DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.exporter.SinkExporter
import ai.privado.model.{CatLevelOne, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.JavaScriptFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class JSS3TaggerTest extends JavaScriptFrontendTestSuite {

  val sinkRule = List(
    RuleInfo(
      "ThirdParties.SDK.AmazonS3",
      "Amazon Aws S3",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i)(@aws-sdk\\/client-s3).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      catLevelTwo = Constants.storages,
      Language.JAVASCRIPT,
      Array()
    )
  )

  val ruleCache = RuleCache().setRule(
    RuleInfoTestData.rule
      .copy(sinks = sinkRule)
  )

  val privadoInput           = new PrivadoInput()
  val s3DatabaseDetailsCache = new S3DatabaseDetailsCache()
  val appCache               = new AppCache()

  "Amazon S3 tagging" should {
    val cpg = code(
      """
        |const {s3Client, GetObjectCommand} = require('@aws-sdk/client-s3')
        |
        |const s3Config = process.env.config
        |const s3Client = new s3Client(s3Config)
        |
        |module.export = s3Service
        |
        |s3Service.url = async (temp) => {
        |	await s3Client.send(new GetObjectCommand({
        |		Bucket: "bucket_name",
        |		Key: "bucket_key",
        |		Body: "body"
        |	}))
        |}
        |""".stripMargin,
      "config.js"
    )
      .withRuleCache(ruleCache)
      .withPrivadoInput(privadoInput)
      .withS3DatabaseDetailsCache(s3DatabaseDetailsCache)
      .withAppCache(appCache)

    "check sink tagging" in {
      val sendNode = cpg.call.name("send").l
      sendNode.size shouldBe 1

      sendNode.headOption.get.methodFullName shouldBe "@aws-sdk/client-s3:send"

      sendNode.tag.size shouldBe 5
      sendNode.tag.nameExact(Constants.id).head.value shouldBe "ThirdParties.SDK.AmazonS3"
      sendNode.tag.nameExact(Constants.catLevelOne).head.value shouldBe "sinks"
      sendNode.tag.nameExact(Constants.catLevelTwo).head.value shouldBe "storages"
    }

    // TODO need to fix this, S3 not reflecting into final result
    "export should having tag sink" ignore {
      val sinkExporter = new SinkExporter(
        cpg,
        ruleCache,
        privadoInput,
        None,
        s3DatabaseDetailsCache,
        appCache = appCache,
        databaseDetailsCache = DatabaseDetailsCache()
      )

      sinkExporter.getSinks.head.name shouldBe "Amazon Aws S3"
    }
  }
}
