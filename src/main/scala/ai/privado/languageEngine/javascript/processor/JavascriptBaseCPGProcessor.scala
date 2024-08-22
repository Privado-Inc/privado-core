package ai.privado.languageEngine.javascript.processor

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
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{cpgOutputFileName, outputDirectoryName}
import ai.privado.model.CpgWithOutputMap
import ai.privado.utility.StatsRecorder
import io.circe.Json
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class JavascriptBaseCPGProcessor(
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
) extends JavascriptProcessor(
      ruleCache,
      privadoInput.copy(disableDataflowPass = true, disablePostProcessingPass = true),
      sourceRepoLocation,
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

  override val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def tagAndExport(xtocpg: Try[Cpg]): Either[String, CpgWithOutputMap] = {
    xtocpg match {
      case Success(cpg) =>
        try {
          statsRecorder.initiateNewStage("overriden overlay Processing")
          applyOverridenPasses(cpg)
          statsRecorder.endLastStage()

          statsRecorder.initiateNewStage("Run oss data flow")
          applyDataflowAndPostProcessingPasses(cpg)
          statsRecorder.endLastStage()
          statsRecorder.setSupressSubstagesFlag(false)
          applyTaggingAndExport(cpg) match
            case Left(err) =>
              logger.debug(s"Errors captured in scanning : $err")
              Left(err)
            case Right(cpgWithOutputMap) => Right(cpgWithOutputMap)
        } finally {
          if returnClosedCpg then cpg.close() // To not close cpg, and use it further, pass the returnClosedCpg as false
          import java.io.File
          val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
          val cpgFile       = new File(cpgOutputPath)
          statsRecorder.justLogMessage(
            s"Binary file size -- ${cpgFile.length()} in Bytes - ${cpgFile.length() * 0.000001} MB\n\n\n"
          )
        }
      case Failure(exception) =>
        logger.error("Error while parsing the source code!")
        logger.debug("Error : ", exception)
        MetricHandler.setScanStatus(false)
        Left("Error while parsing the source code: " + exception.toString)
    }
  }

  override def applyTaggingAndExport(cpg: Cpg): Either[String, CpgWithOutputMap] = {
    statsRecorder.initiateNewStage("Brewing result")
    val result = applyFinalExport(cpg, TaggerCache(), Map.empty, s3DatabaseDetailsCache, appCache) match {
      case Left(err)        => Left(err)
      case Right(outputMap) => Right(CpgWithOutputMap(cpg, outputMap))
    }
    statsRecorder.endLastStage()
    result
  }

  override protected def applyFinalExport(
    cpg: Cpg,
    taggerCache: TaggerCache,
    dataflowMap: Map[String, Path],
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache
  ): Either[String, Map[String, Json]] = {

    val errorMsgs = ListBuffer[String]()

    fileLinkingReportExport(cpg) match
      case Left(err) =>
        errorMsgs.addOne(err)
        Left(errorMsgs.mkString("\n"))
      case Right(_) => Right(Map.empty)
  }
}
