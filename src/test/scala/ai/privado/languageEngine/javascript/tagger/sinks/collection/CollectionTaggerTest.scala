package ai.privado.languageEngine.javascript.tagger.sinks.collection
import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.javascript.tagger.collection.CollectionTagger
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import ai.privado.model._
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class CollectionTaggerTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]
  val ruleCache           = new RuleCache()

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
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
      Array(),
      List("post|get|all|delete|put|patch|head|subscribe|unsubscribe"),
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
    val cpg = hapiCode("""
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
        |""".stripMargin)
    "hapi tagged" in {

      cpg.call.methodFullName(".*(?:route)").l.size shouldBe 1
      cpg.call.head.code.contains("@hapi/hapi") shouldBe true

      val collectionTagger = new CollectionTagger(cpg, ruleCache)

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
    val cpg = fastifyCode("""
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
        |""".stripMargin)
    "fastify tagged" in {

      cpg.call.methodFullName(".*fastify.get").l.size shouldBe 1
      cpg.call.head.code.contains("fastify") shouldBe true

      val collectionTagger = new CollectionTagger(cpg, ruleCache)

      val call = cpg.call.methodFullName(".*fastify.get").head
      val res  = collectionTagger.getCollectionMethodsCache(call, 89L)
      res should be(true)

    }
  }

  def hapiCode(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / "hapi.js").write(code)
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
    cpg
  }

  def fastifyCode(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / "fastify.js").write(code)
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
    cpg
  }
}
