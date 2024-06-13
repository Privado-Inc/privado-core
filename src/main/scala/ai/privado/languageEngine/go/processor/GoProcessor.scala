package ai.privado.languageEngine.go.processor

import ai.privado.cache.*
import ai.privado.entrypoint.{PrivadoInput, TimeMetric}
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.go.passes.SQLQueryParser
import ai.privado.languageEngine.go.passes.config.GoYamlLinkerPass
import ai.privado.languageEngine.go.passes.orm.ORMParserPass
import ai.privado.languageEngine.go.semantic.Language.tagger
import ai.privado.model.Constants.*
import ai.privado.model.{Constants, CpgWithOutputMap, Language}
import ai.privado.passes.*
import ai.privado.utility.PropertyParserPass
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory

import java.util.Calendar
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class GoProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  returnClosedCpg: Boolean = true,
  propertyFilterCache: PropertyFilterCache,
  databaseDetailsCache: DatabaseDetailsCache = new DatabaseDetailsCache()
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.GO,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg,
      databaseDetailsCache,
      propertyFilterCache
    ) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = {
    val passes = mutable.ListBuffer[CpgPassBase]()

    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
    if (privadoInput.assetDiscovery) {
      val projectRoot = java.nio.file.Paths.get(sourceRepoLocation, Constants.generatedConfigFolderName)
      passes.addOne(new JsonPropertyParserPass(cpg, projectRoot.toString))
    } else {
      passes.addOne(new PropertyParserPass(cpg, sourceRepoLocation, ruleCache, Language.GO, propertyFilterCache))
    }

    passes.addOne(new GoYamlLinkerPass(cpg))
    passes.addOne(new SQLParser(cpg, sourceRepoLocation, ruleCache))
    passes.addOne(new SQLQueryParser(cpg))
    passes.addOne(new ORMParserPass(cpg, ruleCache))

    passes.toList
  }

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
    cpg.runTagger(ruleCache, taggerCache, privadoInput, dataFlowCache, appCache, databaseDetailsCache)
    println(
      s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
    )
  }

  override def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    super.applyDataflowAndPostProcessingPasses(cpg)
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using Golang engine")
    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    createCpgFolder(sourceRepoLocation);

    val cpgconfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)
      .withIgnoredFilesRegex(ruleCache.getExclusionRegex)
      .withFetchDependencies(false)

    val xtocpg = new GoSrc2Cpg()
      .createCpg(cpgconfig)
      .map { cpg =>
        println(
          s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
        )

        logger.info("Applying data flow overlay")
        applyDefaultOverlays(cpg)
        cpg
      }

    tagAndExport(xtocpg)
  }
}
