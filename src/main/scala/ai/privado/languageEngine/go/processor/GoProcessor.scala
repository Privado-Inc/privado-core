package ai.privado.languageEngine.go.processor

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.go.passes.SQLQueryParser
import ai.privado.languageEngine.go.passes.config.GoYamlLinkerPass
import ai.privado.languageEngine.go.passes.orm.ORMParserPass
import ai.privado.languageEngine.go.semantic.Language.tagger
import ai.privado.model.Constants.*
import ai.privado.model.{Constants, CpgWithOutputMap, Language}
import ai.privado.passes.*
import ai.privado.semantic.*
import ai.privado.utility.StatsRecorder
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

class GoProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  statsRecorder: StatsRecorder,
  returnClosedCpg: Boolean = true,
  databaseDetailsCache: DatabaseDetailsCache = new DatabaseDetailsCache(),
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
  fileLinkingMetadata: FileLinkingMetadata = new FileLinkingMetadata(),
  dependencies: List[DependencyInfo]
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.GO,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      statsRecorder,
      returnClosedCpg,
      databaseDetailsCache,
      propertyFilterCache,
      fileLinkingMetadata,
      dependencies
    ) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    super.applyPrivadoPasses(cpg) ++ List(
      {
        if (privadoInput.assetDiscovery)
          new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
        else
          new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.GO, propertyFilterCache)
      },
      new GoYamlLinkerPass(cpg),
      new SQLParser(cpg, sourceRepoLocation, ruleCache),
      new SQLQueryParser(cpg),
      new ORMParserPass(cpg, ruleCache)
    )
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    super.runPrivadoTagger(cpg, taggerCache)
    cpg.runTagger(
      ruleCache,
      taggerCache,
      privadoInput,
      dataFlowCache,
      appCache,
      databaseDetailsCache,
      statsRecorder,
      fileLinkingMetadata
    )
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    statsRecorder.justLogMessage("Processing source code using GO engine")
    statsRecorder.initiateNewStage("Base source processing")

    createCpgFolder(sourceRepoLocation)

    val cpgOutputPath    = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    val excludeFileRegex = ruleCache.getRule.exclusions.flatMap(rule => rule.patterns).mkString("|")

    val cpgconfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)
      .withFetchDependencies(false)
    val xtocpg = new GoSrc2Cpg()
      .createCpg(cpgconfig)
      .map { cpg =>
        statsRecorder.endLastStage()
        statsRecorder.initiateNewStage("Applying default overlays")
        applyDefaultOverlays(cpg)
        statsRecorder.endLastStage()
        cpg
      }

    tagAndExport(xtocpg)
  }
}
