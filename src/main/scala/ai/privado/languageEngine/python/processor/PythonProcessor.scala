package ai.privado.languageEngine.python.processor

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.python.config.PythonConfigPropertyPass
import ai.privado.languageEngine.python.metadata.FileLinkingMetadataPassPython
import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import ai.privado.languageEngine.python.passes.config.PythonPropertyLinkerPass
import ai.privado.languageEngine.python.semantic.Language.*
import ai.privado.model.Constants.*
import ai.privado.model.{Constants, CpgWithOutputMap, Language}
import ai.privado.passes.*
import ai.privado.utility.StatsRecorder
import ai.privado.utility.Utilities.createCpgFolder
import better.files.File
import io.joern.pysrc2cpg.*
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

import java.nio.file.Paths

class PythonProcessor(
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
      Language.PYTHON,
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

  override val logger = LoggerFactory.getLogger(getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    super.applyPrivadoPasses(cpg) ++ List(
      new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput), {
        if (privadoInput.assetDiscovery) {
          new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
          new PythonConfigPropertyPass(cpg)
        } else {
          new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.PYTHON, propertyFilterCache)
        }
      },
      new PythonPropertyLinkerPass(cpg),
      new SQLParser(cpg, sourceRepoLocation, ruleCache),
      new SQLPropertyPass(cpg, sourceRepoLocation, ruleCache),
      new DBTParserPass(cpg, sourceRepoLocation, ruleCache, databaseDetailsCache)
    )
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    super.runPrivadoTagger(cpg, taggerCache)
    cpg.runTagger(
      ruleCache,
      taggerCache,
      privadoInput,
      dataFlowCache,
      s3DatabaseDetailsCache,
      appCache,
      databaseDetailsCache,
      statsRecorder,
      fileLinkingMetadata
    )
  }

  override def applyOverridenPasses(cpg: Cpg): Unit = {
    new ImportsPass(cpg).createAndApply()
    new PythonImportResolverPass(cpg).createAndApply()
    new PythonInheritanceNamePass(cpg).createAndApply()
    new DynamicTypeHintFullNamePass(cpg).createAndApply()

    new PythonTypeRecoveryPassGenerator(cpg).generate().foreach(_.createAndApply())
    new PrivadoPythonTypeHintCallLinker(cpg).createAndApply()
    new NaiveCallLinker(cpg).createAndApply()
    new AstLinkerPass(cpg).createAndApply()

    if (privadoInput.fileLinkingReport) {
      new FileLinkingMetadataPassPython(cpg, fileLinkingMetadata).createAndApply()
    }
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    statsRecorder.justLogMessage("Processing source code using Python engine")
    statsRecorder.initiateNewStage("Base source processing")

    createCpgFolder(sourceRepoLocation)
    // Converting path to absolute path, we may need that same as JS
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath
    val cpgOutputPath          = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"

    val excludeFileRegex = ruleCache.getExclusionRegex
    // TODO Discover ignoreVenvDir and set ignore true or flase based on user input
    val cpgconfig = Py2CpgOnFileSystemConfig(Option(File(".venv").path), ignoreVenvDir = true)
      .withInputPath(absoluteSourceLocation.toString)
      .withOutputPath(Paths.get(cpgOutputPath).toString)
      .withIgnoredFilesRegex(excludeFileRegex)
    val xtocpg = new Py2CpgOnFileSystem().createCpg(cpgconfig).map { cpg =>
      statsRecorder.endLastStage()
      statsRecorder.justLogMessage(s"Total no of graph nodes -> ${cpg.graph.nodeCount()}")
      statsRecorder.initiateNewStage("Applying default overlays")
      applyDefaultOverlays(cpg)
      statsRecorder.endLastStage()
      cpg
    }

    tagAndExport(xtocpg)
  }
}
