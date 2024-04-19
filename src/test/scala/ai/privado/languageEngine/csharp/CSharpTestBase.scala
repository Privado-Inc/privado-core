package ai.privado.languageEngine.csharp

import ai.privado.cache.{AuditCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.*
import ai.privado.threatEngine.ThreatEngineExecutor
import better.files.File
import io.joern.csharpsrc2cpg.{CSharpSrc2Cpg, Config}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import ai.privado.languageEngine.csharp.tagger.collection.CollectionTagger

import scala.collection.mutable
import ai.privado.cache.*
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.layers.*
import io.joern.dataflowengineoss.layers.dataflows.*
import ai.privado.languageEngine.csharp.tagger.source.IdentifierTagger
import ai.privado.model.SourceCodeModel
import ai.privado.rule.RuleInfoTestData
import ai.privado.tagger.source.LiteralTagger

abstract class CSharpTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]

  val sinkRules: List[RuleInfo] = List(
    RuleInfo(
      "Loggers.Console",
      "WriteLine",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*WriteLine.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "",
      Language.CSHARP,
      Array()
    )
  )

  val collectionRules = List(
    RuleInfo(
      "Collections.Mvc",
      "ASPNet MVC Endpoints",
      "",
      FilterProperty.CODE,
      Array(),
      List("(?i).*(Route|HttpGet|HttpPost|HttpPut).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = Constants.annotations,
      Language.CSHARP,
      Array()
    )
  )

  val configAndRules: ConfigAndRules =
    ConfigAndRules(
      RuleInfoTestData.sourceRule,
      sinkRules,
      collectionRules,
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      List()
    )

  val taggerCache = new TaggerCache()

  def code(sourceCodes: List[SourceCodeModel]): (Cpg, ThreatEngineExecutor) = {
    val ruleCache                    = new RuleCache()
    val dataFlows: Map[String, Path] = Map()
    val auditCache                   = new AuditCache
    val privadoInput                 = PrivadoInput()
    val dataFlowCache                = new DataFlowCache(privadoInput, auditCache)

    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    for (sourceCode <- sourceCodes) {
      (inputDir / sourceCode.fileName).write(sourceCode.sourceCode)
    }
    val outputFile: File = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val config = Config()
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)

    ruleCache.withRule(configAndRules)
    val cpg      = new CSharpSrc2Cpg().createCpg(config).get
    val appCache = new AppCache()
    appCache.repoLanguage = Language.CSHARP

    X2Cpg.applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new LiteralTagger(cpg, ruleCache).createAndApply()
    new CollectionTagger(cpg, ruleCache).createAndApply()

    cpgs.addOne(cpg)
    val threatEngine =
      new ThreatEngineExecutor(
        cpg,
        config.inputPath,
        ruleCache,
        null,
        dataFlowCache.getDataflowAfterDedup,
        privadoInput,
        appCache = appCache
      )
    (cpg, threatEngine)
  }

}
