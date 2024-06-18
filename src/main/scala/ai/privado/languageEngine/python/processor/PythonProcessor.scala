package ai.privado.languageEngine.python.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.*
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.python.config.PythonConfigPropertyPass
import ai.privado.languageEngine.python.passes.PrivadoPythonTypeHintCallLinker
import ai.privado.languageEngine.python.passes.config.PythonPropertyLinkerPass
import ai.privado.languageEngine.python.semantic.Language.*
import ai.privado.languageEngine.python.tagger.PythonS3Tagger
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.{CatLevelOne, Constants, CpgWithOutputMap, Language}
import ai.privado.passes.*
import ai.privado.semantic.Language.*
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass, UnresolvedReportUtility}
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.pysrc2cpg.*
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Paths
import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class PythonProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  returnClosedCpg: Boolean = true,
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
  databaseDetailsCache: DatabaseDetailsCache = new DatabaseDetailsCache()
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
      databaseDetailsCache,
      propertyFilterCache
    ) {

  override val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    List(
      new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput), {
        if (privadoInput.assetDiscovery) {
          new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
          new PythonConfigPropertyPass(cpg)
        } else {
          new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.PYTHON, propertyFilterCache)
        }
      },
      new ImportsPass(cpg),
      new PythonImportResolverPass(cpg),
      new PythonInheritanceNamePass(cpg),
      new DynamicTypeHintFullNamePass(cpg)
    ) ++
      new PythonTypeRecoveryPassGenerator(cpg).generate() ++
      List(
        new PrivadoPythonTypeHintCallLinker(cpg),
        new NaiveCallLinker(cpg),
        new AstLinkerPass(cpg),
        new PythonPropertyLinkerPass(cpg),
        new SQLParser(cpg, sourceRepoLocation, ruleCache),
        new SQLPropertyPass(cpg, sourceRepoLocation, ruleCache),
        new DBTParserPass(cpg, sourceRepoLocation, ruleCache, databaseDetailsCache)
      ) ++ {
        if (privadoInput.enableLambdaFlows) {
          List(new ExperimentalLambdaDataFlowSupportPass(cpg))
        } else {
          List()
        }
      }
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    cpg.runTagger(
      ruleCache,
      taggerCache,
      privadoInput,
      dataFlowCache,
      s3DatabaseDetailsCache,
      appCache,
      databaseDetailsCache
    )
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using Python engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

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
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      applyDefaultOverlays(cpg)
      cpg
    }

    tagAndExport(xtocpg)
  }
}
