package ai.privado.languageEngine.javascript.passes.read

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.languageEngine.javascript.tagger.source.IdentifierTagger
import ai.privado.languageEngine.javascript.passes.read.GraphqlQueryParserPass
import ai.privado.model.*
import better.files.File
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class GraphqlQueryParserPassTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]
  val ruleCache           = new RuleCache()

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
    ),
    RuleInfo(
      "Data.Sensitive.Email",
      "Email",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*email.*"),
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

  val sinkRule = List()

  val systemConfig = List(
    SystemConfig(
      "apiGraphqlLibraries",
      "(?i)(.*ApolloClient|graphql|express-graphql).*",
      Language.JAVASCRIPT,
      "",
      Array()
    ),
    SystemConfig("apiGraphqlReadSink", "(?i)(fetchapi|fetchlegacyxml|get)", Language.JAVASCRIPT, "", Array()),
    SystemConfig(
      "apiGraphqlWriteSink",
      "(?i)(axios|cors|set|post|put|patch|Path|send|sendAsync|remove|delete|write|read)",
      Language.JAVASCRIPT,
      "",
      Array()
    )
  )

  "Graphql Query parser with buildSchema" should {
    val cpg = graphqlCode("""
                            |const { graphql, buildSchema } = require('graphql');
                            |
                            |// Builds a schema using the GraphQL schema language
                            |const schema = buildSchema(`
                            |  type User {
                            |    firstName: String!
                            |    lastName: String!
                            |    accountId: String!
                            |  }
                            |`);
                            |
                            |""".stripMargin)
    "Source should be tagged" in {
      val literalNode = cpg.call("buildSchema").argument.isLiteral.head
      literalNode.astParent.isCall shouldBe true
      literalNode.tag.size shouldBe 6
      literalNode.tag.nameExact(Constants.id).head.value shouldBe "Data.Sensitive.FirstName"
      literalNode.tag.nameExact(Constants.catLevelOne).head.value shouldBe Constants.sources
      literalNode.tag.nameExact(Constants.nodeType).head.value shouldBe "REGULAR"
    }
  }

  "Graphql query parser with literal starting with query & mutation" should {
    val cpg = graphqlCode("""
        |const { graphql, buildSchema } = require('graphql');
        |requestBody = {
        |  query: `
        |    mutation CreateUser($email: String!, $password: String!) {
        |      createUser(userInput: {email: $email, password: $password}) {
        |        _id
        |        email
        |      }
        |    }
        |  `,
        |  variables: {
        |    email: email,
        |    password: password
        |  }
        |};
        |""".stripMargin)
    "Source should be tagged" in {
      val literalNode = cpg.literal.code(".*mutation.*").head
      literalNode.astParent.isCall shouldBe true
      literalNode.tag.size shouldBe 6
      literalNode.tag.nameExact(Constants.id).head.value shouldBe "Data.Sensitive.Email"
      literalNode.tag.nameExact(Constants.catLevelOne).head.value shouldBe Constants.sources
      literalNode.tag.nameExact(Constants.nodeType).head.value shouldBe "REGULAR"
    }
  }

  def graphqlCode(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / "graphql-example.js").write(code)
    val outputFile = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val rule: ConfigAndRules =
      ConfigAndRules(sourceRule, sinkRule, List(), List(), List(), List(), List(), List(), systemConfig, List())
    val ruleCache = new RuleCache()
    ruleCache.withRule(rule)
    val config      = Config().withInputPath(inputDir.toString()).withOutputPath(outputFile.toString())
    val cpg         = new JsSrc2Cpg().createCpgWithAllOverlays(config).get
    val taggerCache = new TaggerCache()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new GraphqlQueryParserPass(cpg, ruleCache, taggerCache).createAndApply()
    cpgs.addOne(cpg)
    cpg
  }
}
