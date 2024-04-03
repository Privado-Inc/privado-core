package ai.privado.languageEngine.kotlin.processor

import ai.privado.audit.{AuditReportEntryPoint, DependencyReport}
import ai.privado.cache.{
  AppCache,
  AuditCache,
  DataFlowCache,
  PropertyFilterCache,
  RuleCache,
  S3DatabaseDetailsCache,
  TaggerCache
}
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.languageEngine.java.passes.config.{JavaPropertyLinkerPass, ModuleFilePass}
import ai.privado.languageEngine.java.passes.module.{DependenciesCategoryPass, DependenciesNodePass}
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{
  cpgOutputFileName,
  outputAuditFileName,
  outputDirectoryName,
  outputFileName,
  outputIntermediateFileName,
  outputUnresolvedFilename
}
import ai.privado.model.{CatLevelOne, Constants, CpgWithOutputMap, Language}
import ai.privado.passes.{
  AndroidXmlParserPass,
  DBTParserPass,
  ExperimentalLambdaDataFlowSupportPass,
  HTMLParserPass,
  JsonPropertyParserPass,
  SQLParser,
  SQLPropertyPass
}
import ai.privado.semantic.Language.*
import ai.privado.languageEngine.kotlin.semantic.Language.*
import ai.privado.model.Language.Language
import ai.privado.utility.{PropertyParserPass, UnresolvedReportUtility}
import ai.privado.utility.Utilities.createCpgFolder
import better.files.File
import io.circe.Json
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.kotlin2cpg.Kotlin2Cpg
import io.joern.kotlin2cpg.Config
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
class KotlinProcessor(
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
      Language.KOTLIN,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg
    ) {
  override val logger   = LoggerFactory.getLogger(getClass)
  private var cpgconfig = Config()

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    List({
      if (privadoInput.assetDiscovery)
        new JsonPropertyParserPass(cpg, s"$sourceRepoLocation/${Constants.generatedConfigFolderName}")
      else
        new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.JAVA, propertyFilterCache)
    }) ++
      List(
        new JavaPropertyLinkerPass(cpg),
        new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = privadoInput),
        new SQLParser(cpg, sourceRepoLocation, ruleCache),
        new SQLPropertyPass(cpg, sourceRepoLocation, ruleCache),
        new AndroidXmlParserPass(cpg, sourceRepoLocation, ruleCache)
      )
  }

  override def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    super.applyDataflowAndPostProcessingPasses(cpg)
    Kotlin2Cpg.postProcessingPass(cpg)
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit =
    cpg.runTagger(ruleCache, taggerCache, privadoInputConfig = privadoInput, dataFlowCache, appCache)

  override def processCpg(): Either[String, CpgWithOutputMap] = {

    println(s"${Calendar.getInstance().getTime} - Processing source code using Kotlin engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);
    val excludeFileRegex = ruleCache.getExclusionRegex

    val cpgconfig = Config(includeJavaSourceFiles = true)
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(excludeFileRegex)

    val xtocpg = new Kotlin2Cpg().createCpg(cpgconfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      // Apply default overlays
      X2Cpg.applyDefaultOverlays(cpg)
      cpg
    }
    tagAndExport(xtocpg) match {
      case Left(msg)               => Left(msg)
      case Right(cpgWithOutputMap) => Right(cpgWithOutputMap)
    }
  }

}
