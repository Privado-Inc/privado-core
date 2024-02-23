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
import scala.collection.mutable
import ai.privado.cache.*
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.layers.*
import io.joern.dataflowengineoss.layers.dataflows.*
import ai.privado.languageEngine.csharp.tagger.source.IdentifierTagger

abstract class CSharpTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.PersonalIdentification.FirstName",
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
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.PersonalIdentification.LastName",
      "LastName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*lastName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.PersonalIdentification.DateofBirth",
      "Date of Birth",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*dob.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.ContactData.EmailAddress",
      "EmailAddress",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*email.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.ContactData.PhoneNumber",
      "Phone",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*phone.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.FinancialData.Salary",
      "Salary",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*salary.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    )
  )

  val configAndRules: ConfigAndRules =
    ConfigAndRules(sourceRule, List(), List(), List(), List(), List(), List(), List(), List(), List())

  val taggerCache = new TaggerCache()

  def code(code: String, fileExtension: String = ".cs"): (Cpg, ThreatEngineExecutor) = {
    val ruleCache                    = new RuleCache()
    val dataFlows: Map[String, Path] = Map()
    val auditCache                   = new AuditCache
    val privadoInput                 = PrivadoInput()
    val dataFlowCache                = new DataFlowCache(privadoInput, auditCache)

    val inputDir = File.newTemporaryDirectory()
    inputDirs.addOne(inputDir)
    (inputDir / s"generalFile${fileExtension}").write(code)
    val outputFile: File = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val config = Config()
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)

    ruleCache.setRule(configAndRules)
    val cpg = new CSharpSrc2Cpg().createCpg(config).get
    AppCache.repoLanguage = Language.CSHARP

    X2Cpg.applyDefaultOverlays(cpg)
    val context = new LayerCreatorContext(cpg)
    val options = new OssDataFlowOptions()
    new OssDataFlow(options).run(context)
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    cpgs.addOne(cpg)
    val threatEngine =
      new ThreatEngineExecutor(
        cpg,
        config.inputPath,
        ruleCache,
        null,
        dataFlowCache.getDataflowAfterDedup,
        privadoInput
      )
    (cpg, threatEngine)
  }

}
