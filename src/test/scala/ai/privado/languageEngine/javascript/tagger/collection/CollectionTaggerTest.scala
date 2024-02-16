package ai.privado.languageEngine.javascript.tagger.collection

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.exporter.CollectionExporter
import ai.privado.languageEngine.javascript.tagger.collection.CollectionTagger
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import ai.privado.model.*
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class CollectionTaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*firstName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVASCRIPT,
      Array()
    )
  )

  val collectionRule = List(
    RuleInfo(
      "Collections.Express",
      "Express framework restendpoint",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*express.*(post|get|all|delete|put|patch|head|subscribe|unsubscribe)"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = Constants.default,
      Language.JAVASCRIPT,
      Array()
    )
  )

  "Simple Hapi sample" should {
    val (cpg: Cpg, ruleInfo: RuleCache) = code(
      """
        |const Hapi = require('@hapi/hapi');
        |const Joi = require('@hapi/joi');
        |Joi.objectId = require('joi-objectid')(Joi)
        |
        |const init = async () => {
        |
        |    const server = Hapi.server({
        |        port: 3000,
        |        host: 'localhost'
        |    });
        |
        |    await server.register({
        |        plugin: require('hapi-mongodb'),
        |        options: {
        |          url: '{YOUR-CONNECTION-STRING}',
        |          settings: {
        |              useUnifiedTopology: true
        |          },
        |          decorate: true
        |        }
        |    });
        |    // Get a single movie
        |    server.route({
        |        method: 'GET',
        |        path: '/movies/{id}',
        |        handler: async (req, h) => {
        |            const id = req.params.id
        |            const ObjectID = req.mongo.ObjectID;
        |
        |            const firstName = await req.mongo.db.collection('firstName').findOne({_id: new ObjectID(id)},{projection:{title:1,plot:1,cast:1,year:1, released:1}});
        |
        |            return firstName;
        |        }
        |    });
        |
        |    await server.start();
        |    console.log('Server running on %s', server.info.uri);
        |}
        |
        |init();
        |""".stripMargin,
      "hapi.js"
    )
    "hapi tagged" in {

      cpg.call.methodFullName(".*(?:route)").l.size shouldBe 1
      cpg.call.head.code.contains("@hapi/hapi") shouldBe true

      val collectionTagger = new CollectionTagger(cpg, ruleInfo)

      val input          = "abc+def+xyz"
      val output         = collectionTagger.convertAdditionString(input)
      val expectedOutput = "abcdefxyz"
      assert(output == expectedOutput)

      val input2          = "<operator>.formatString('Hello, {}!', name)"
      val output2         = collectionTagger.convertFormatString(input2)
      val expectedOutput2 = "Hello{}!name"
      assert(output2 == expectedOutput2)

      val block        = cpg.call.methodFullName("(hapi|@hapi/hapi).*(?:route)").l.argument.isBlock.head
      val result       = collectionTagger.getRouteAndHandlerFromBlock(block, "path", "handler")
      val expectedPath = "\"/movies/{id}\""
      result._2.methodRef.size shouldBe 1
      result._1 shouldEqual expectedPath

    }
  }

  "Simple Fastify sample" should {
    val (cpg: Cpg, ruleCache: RuleCache) = code(
      """
        |const fastify = require('fastify')({ logger: true });
        |
        |//route
        |fastify.get('/', function (request, reply) {
        |    reply.send({ hello: 'world' });
        |});
        |
        |// Run the server!
        |fastify.listen(3000, function (err, address) {
        |    if (err) {
        |        fastify.log.error(err);
        |        process.exit(1);
        |    }
        |    fastify.log.info(`server listening on ${address}`);
        |})
        |""".stripMargin,
      fileName = "fastify.js"
    )
    "fastify tagged" in {

      cpg.call.methodFullName(".*fastify.get").l.size shouldBe 1
      cpg.call.head.code.contains("fastify") shouldBe true

      val collectionTagger = new CollectionTagger(cpg, ruleCache)

      val call = cpg.call.methodFullName(".*fastify.get").head
      val res  = collectionTagger.getCollectionMethodsCache(call, 89L)
      res should be(true)

    }
  }

  "Collection point with handler passed as function" should {
    val (cpg: Cpg, ruleCache: RuleCache) = code(
      """
        |import express from "express";
        |const app = express();
        |
        |const profileService = ProfileService();
        |app.post(
        |    `${basePath}/payment-activations`,
        |    bearerSessionTokenAuth,
        |    toExpressHandler(
        |      getUserForFIMS(profileService)
        |    )
        |  );
        |
        |
        |
        |const getUserForFIMS =
        |  (profileService: ProfileService) =>
        |  (
        |    req: express.Request
        |  ): Promise<
        |    | IResponseErrorValidation
        |    | IResponseErrorInternal
        |    | IResponseErrorTooManyRequests
        |    | IResponseErrorNotFound
        |    | IResponseSuccessJson<FIMSUser>
        |  > =>
        |    withUserFromRequest(req, async (user) =>
        |      pipe(
        |        TE.tryCatch(
        |          () => profileService.getProfile(user),
        |          () => ResponseErrorInternal("Cannot retrieve profile")
        |        ),
        |        TE.chain((r) =>
        |          r.kind === "IResponseSuccessJson" ? TE.of(r.value) : TE.left(r)
        |        ),
        |        TE.chainW((userProfile) =>
        |          TE.fromEither(
        |            pipe(
        |              FIMSUser.decode({
        |                acr: user.spid_level,
        |                auth_time: user.created_at,
        |                firstName: user.firstName,
        |                // If the email is not validated yet, the value returned will be undefined
        |                email: pipe(
        |                  O.fromNullable(userProfile.is_email_validated),
        |                  O.chain(fromPredicate(identity)),
        |                  O.chain(() => O.fromNullable(userProfile.email)),
        |                  O.toUndefined
        |                ),
        |                family_name: user.family_name,
        |                fiscal_code: user.fiscal_code,
        |                name: user.name,
        |              }),
        |              E.mapLeft((_) =>
        |                ResponseErrorInternal(errorsToReadableMessages(_).join(" / "))
        |              )
        |            )
        |          )
        |        ),
        |        TE.map(ResponseSuccessJson),
        |        TE.toUnion
        |      )()
        |);
        |
        |
        |""".stripMargin,
      "controller.js"
    )

    new CollectionTagger(cpg, ruleCache).createAndApply()
    val collectionExporter = new CollectionExporter(cpg, ruleCache)

    "have collection exported" in {
      val collectionModel :: _ = collectionExporter.getCollections.l

      collectionModel.collectionId shouldBe ("Collections.Express")
      val collectionOcc :: _ = collectionModel.collections.l
      collectionOcc.sourceId shouldBe ("Data.Sensitive.FirstName")
      val collectionOccModel :: _ = collectionOcc.occurrences.l
      collectionOccModel.endPoint shouldBe ("basePath/payment-activations")
    }
  }

  val (cpg: Cpg, ruleCache: RuleCache) = code(
    """
      |const Hapi = require('@hapi/hapi');
      |const Joi = require('@hapi/joi');
      |Joi.objectId = require('joi-objectid')(Joi)
      |
      |const init = async () => {
      |
      |    const server = Hapi.server({
      |        port: 3000,
      |        host: 'localhost'
      |    });
      |
      |    await server.register({
      |        plugin: require('hapi-mongodb'),
      |        options: {
      |          url: '{YOUR-CONNECTION-STRING}',
      |          settings: {
      |              useUnifiedTopology: true
      |          },
      |          decorate: true
      |        }
      |    });
      |    // Get a single movie
      |    server.route({
      |        method: 'GET',
      |        path: '/movies/{id}',
      |        handler: async (req, h) => {
      |            const id = req.params.id
      |            const ObjectID = req.mongo.ObjectID;
      |
      |            const firstName = await req.mongo.db.collection('firstName').findOne({_id: new ObjectID(id)},{projection:{title:1,plot:1,cast:1,year:1, released:1}});
      |
      |            return firstName;
      |        }
      |    });
      |
      |    await server.start();
      |    console.log('Server running on %s', server.info.uri);
      |}
      |
      |init();
      |""".stripMargin,
    "hapi.js"
  )
  "test convertAdditionString" should {
    val collectionTagger = new CollectionTagger(cpg, ruleCache)

    "convertAdditionString should remove '+' characters" in {
      val input    = "'/base/' + customerId + '/page/1'"
      val expected = "/base/customerId/page/1"
      val result   = collectionTagger.convertAdditionString(input)
      assert(result == expected)
    }

    "convertAdditionString should remove single and double quotes" in {
      val input    = "It's a 'test'"
      val expected = "Itsatest"
      val result   = collectionTagger.convertAdditionString(input)
      assert(result == expected)
    }

    "convertAdditionString should remove backticks" in {
      val input    = "`code`"
      val expected = "code"
      val result   = collectionTagger.convertAdditionString(input)
      assert(result == expected)
    }

    "convertAdditionString should remove spaces" in {
      val input    = "  1  +  2    +  3   "
      val expected = "123"
      val result   = collectionTagger.convertAdditionString(input)
      assert(result == expected)
    }

    "convertAdditionString should handle an empty string" in {
      val input    = ""
      val expected = ""
      val result   = collectionTagger.convertAdditionString(input)
      assert(result == expected)
    }
  }

  "test convertFormatString" should {
    val collectionTagger = new CollectionTagger(cpg, ruleCache)

    "convertFormatString should remove '<operator>.formatString('" in {
      val input    = "<operator>.formatString('Hello, %s!', name)"
      val expected = "Hello%s!name"
      val result   = collectionTagger.convertFormatString(input)
      assert(result == expected)
    }

    "convertFormatString should handle an empty string" in {
      val input    = ""
      val expected = ""
      val result   = collectionTagger.convertFormatString(input)
      assert(result == expected)
    }
  }

  def code(code: String, fileName: String): (Cpg, RuleCache) = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / fileName).write(code)
    val outputFile = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val rule: ConfigAndRules =
      ConfigAndRules(sourceRule, List(), collectionRule, List(), List(), List(), List(), List(), List(), List())
    val ruleCache = new RuleCache()
    ruleCache.setRule(rule)
    val config      = Config().withInputPath(inputDir.toString()).withOutputPath(outputFile.toString())
    val cpg         = new JsSrc2Cpg().createCpgWithAllOverlays(config).get
    val taggerCache = new TaggerCache()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    cpgs.addOne(cpg)
    (cpg, ruleCache)
  }
}
