package ai.privado.languageEngine.python.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.*
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.languageEngine.base.processor.BaseProcessor
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import ai.privado.languageEngine.python.config.PythonConfigPropertyPass
import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import ai.privado.languageEngine.python.passes.config.PythonPropertyLinkerPass
import ai.privado.languageEngine.python.semantic.Language.*
import ai.privado.languageEngine.python.tagger.PythonS3Tagger
import ai.privado.model.Constants.*
import ai.privado.model.{Constants, Language, CpgWithOutputMap}
import ai.privado.passes.{DBTParserPass, HTMLParserPass, JsonPropertyParserPass, SQLParser, SQLPropertyPass}
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass}
import better.files.File
import io.joern.pysrc2cpg.*
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph.generated.{Cpg}
import io.shiftleft.passes.CpgPassBase

import org.slf4j.{Logger, LoggerFactory}

import java.util.Calendar

class PythonProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  returnClosedCpg: Boolean = true,
  propertyFilterCache: PropertyFilterCache
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.PYTHON,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg,
      propertyFilterCache
    ) {
  override val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    val passesList = List(
      new ImportsPass(cpg),
      new PythonImportResolverPass(cpg),
      new PythonInheritanceNamePass(cpg),
      new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput),
      new DynamicTypeHintFullNamePass(cpg)
    )

    passesList ++ new PythonTypeRecoveryPassGenerator(cpg).generate() ++ List(
      new PrivadoPythonTypeHintCallLinker(cpg),
      new NaiveCallLinker(cpg),
      new AstLinkerPass(cpg)
    ) ++ List({
      if (privadoInput.assetDiscovery)
        new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
        new PythonConfigPropertyPass(cpg)
      else
        new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.PYTHON, propertyFilterCache, privadoInput)
    }) ++ List(
      new PythonPropertyLinkerPass(cpg),
      new SQLParser(cpg, sourceRepoLocation, ruleCache),
      new SQLPropertyPass(cpg, sourceRepoLocation, ruleCache),
      new DBTParserPass(cpg, sourceRepoLocation, ruleCache)
    )
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit =
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache, appCache)
    new PythonS3Tagger(cpg, s3DatabaseDetailsCache).createAndApply()

  override def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    super.applyDataflowAndPostProcessingPasses(cpg)

  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using Python engine")

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    // TODO Discover ignoreVenvDir and set ignore true or flase based on user input
    val cpgConfig = Py2CpgOnFileSystemConfig(Option(File(".venv").path), ignoreVenvDir = true)
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(ruleCache.getExclusionRegex)

    val xtocpg = new Py2CpgOnFileSystem().createCpg(cpgConfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      applyDefaultOverlays(cpg)
      cpg
    }

    tagAndExport(xtocpg)
  }

}
