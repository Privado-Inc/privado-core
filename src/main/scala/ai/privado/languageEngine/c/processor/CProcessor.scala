package ai.privado.languageEngine.c.processor

import ai.privado.cache.{
  AppCache,
  AuditCache,
  DataFlowCache,
  DatabaseDetailsCache,
  FileLinkingMetadata,
  PropertyFilterCache,
  RuleCache,
  S3DatabaseDetailsCache,
  TaggerCache
}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.c.passes.{CImportResolverPass, CTypeHintCallLinker, CTypeRecoveryPassGenerator}
import ai.privado.model.Constants.{cpgOutputFileName, outputDirectoryName}
import ai.privado.model.{CpgWithOutputMap, Language}
import ai.privado.utility.StatsRecorder
import ai.privado.utility.Utilities.createCpgFolder
import org.slf4j.LoggerFactory
import io.joern.c2cpg.*
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.languageEngine.c.semantic.Language.tagger

class CProcessor(
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
  fileLinkingMetadata: FileLinkingMetadata = new FileLinkingMetadata()
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.C,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      statsRecorder,
      returnClosedCpg,
      databaseDetailsCache,
      propertyFilterCache,
      fileLinkingMetadata
    ) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
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
    statsRecorder.justLogMessage("Processing source code using C engine")
    statsRecorder.initiateNewStage("Base source processing")
    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);
    val excludeFileRegex = ruleCache.getExclusionRegex

    val cpgconfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)
      .withSkipHeaderFileContext(privadoInput.isSkipHeaderFileContext)

    val xtocpg = new C2Cpg().createCpg(cpgconfig).map { cpg =>
      statsRecorder.endLastStage()
      statsRecorder.initiateNewStage("Default overlays")
      // Apply default overlays
      X2Cpg.applyDefaultOverlays(cpg)
      statsRecorder.endLastStage()
      cpg
    }
    tagAndExport(xtocpg)
  }

  override def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    super.applyDataflowAndPostProcessingPasses(cpg)
    new CImportResolverPass(cpg).createAndApply()
    new CTypeRecoveryPassGenerator(cpg = cpg).generate().foreach(_.createAndApply())
    new CTypeHintCallLinker(cpg).createAndApply()
  }
}
