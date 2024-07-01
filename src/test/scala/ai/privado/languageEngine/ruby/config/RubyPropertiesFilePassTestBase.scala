package ai.privado.languageEngine.ruby.config

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.ruby.passes.config.RubyEnvPropertyLinkerPass
import ai.privado.languageEngine.ruby.tagger.sink.APITagger
import ai.privado.languageEngine.ruby.tagger.source.IdentifierTagger
import ai.privado.model.{
  CatLevelOne,
  ConfigAndRules,
  Constants,
  FilterProperty,
  Language,
  NodeType,
  RuleInfo,
  SystemConfig
}
import ai.privado.utility.{PropertyParserPass, StatsRecorder}
import better.files.File
import io.joern.rubysrc2cpg.{Config, RubySrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class RubyPropertiesFilePassTestBase(fileExtension: String)
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll {
  var cpg: Cpg = _
  val configFileContents: String
  val codeFileContents: String
  var inputDir: File  = _
  var outputDir: File = _
  val ruleCache       = new RuleCache()

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / s"config$fileExtension").write(configFileContents)
    (inputDir / s"code.rb").write(codeFileContents)
    outputDir = File.newTemporaryDirectory()

    val config = Config()
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputDir.pathAsString)
      .withUseDeprecatedFrontend(true)
    val rubySrc = new RubySrc2Cpg()
    val xtocpg = rubySrc.createCpg(config).map { cpg =>
      applyDefaultOverlays(cpg)
      cpg
    }

    ruleCache.setRule(rule)

    cpg = xtocpg.get
    new PropertyParserPass(cpg, inputDir.pathAsString, new RuleCache(), Language.RUBY).createAndApply()
    new RubyEnvPropertyLinkerPass(cpg).createAndApply()
    new IdentifierTagger(cpg, ruleCache).createAndApply()
    new APITagger(cpg, ruleCache, PrivadoInput(), AppCache(), StatsRecorder()).createAndApply()
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputDir.delete()
    super.afterAll()
  }

  val systemConfig = List(
    SystemConfig(
      "apiHttpLibraries",
      "(?i)(multipart|faraday|rest-client|httparty|http.client|net.http|curb|sawyer|unirest|excon|typhoeus|.*(Http(.){0,2}Client|RestClient|HTTParty|Faraday|Unirest)).*",
      Language.RUBY,
      "",
      Array()
    ),
    SystemConfig(
      "apiSinks",
      "(?i)(?:new|url|client|openConnection|request|execute|newCall|load|host|access|usequery|fetch|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity|call|createCall|createEndpoint|dispatch|invoke|newMessage|getInput|getOutput|getResponse|marshall|unmarshall|send|asyncSend|emit)",
      Language.RUBY,
      "",
      Array()
    ),
    SystemConfig(
      "apiIdentifier",
      "(?i).*((hook|base|auth|prov|endp|install|cloud|host|request|service|gateway|route|resource|upload|api|worker)(.){0,12}url|(slack|web)(.){0,4}hook|(sentry|segment)(.){0,1}(dsn)|(rest|api|host|cloud|request|service)(.){0,4}(endpoint|gateway|route)).*",
      Language.RUBY,
      "",
      Array()
    ),
    SystemConfig(
      "ignoredSinks",
      "((?i).*(?<=map|list|jsonobject|json|array|arrays|jsonnode|objectmapper|objectnode).*(put:|get:).*",
      Language.RUBY,
      "",
      Array()
    )
  )

  val sinkRule = List(
    RuleInfo(
      Constants.thirdPartiesAPIRuleId,
      "Third Party API",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(
        "(?i)((?:http|https):\\/\\/[a-zA-Z0-9_-][^)\\/(#|,!>\\s]{1,50}\\.\\b(?:com|net|org|de|in|uk|us|io|gov|cn|ml|ai|ly|dev|cloud|me|icu|ru|info|top|tk|tr|cn|ga|cf|nl)\\b).*(?<!png|jpeg|jpg|txt|blob|css|html|js|svg)",
        "(?i).*((hook|base|auth|prov|endp|install|request|service|gateway|route|resource)(.){0,12}url|(slack|web)(.){0,4}hook|(rest|api|request|service)(.){0,4}(endpoint|gateway|route)).*"
      ),
      false,
      "",
      Map(),
      NodeType.API,
      "",
      CatLevelOne.SINKS,
      catLevelTwo = Constants.third_parties,
      Language.RUBY,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sinks = sinkRule, systemConfig = systemConfig)
}
