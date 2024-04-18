/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.languageEngine.default.processor

import ai.privado.cache.*
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.ScanProcessor
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.exporter.JSONExporter
import ai.privado.languageEngine.default.semantic.Language.*
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.*
import ai.privado.model.{CatLevelOne, Constants, Language}
import ai.privado.passes.{DBTParserPass, HTMLParserPass, SQLParser}
import ai.privado.semantic.Language.*
import ai.privado.tagger.source.SqlQueryTagger
import ai.privado.utility.Utilities.createCpgFolder
import ai.privado.utility.{PropertyParserPass, StatsRecorder, UnresolvedReportUtility}
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.Config as JavaConfig
import io.joern.x2cpg.X2Cpg.{applyDefaultOverlays, withNewEmptyCpg}
import io.joern.x2cpg.utils.ExternalCommand
import io.joern.x2cpg.utils.dependency.DependencyResolver
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class DefaultProcessor(statsRecorder: StatsRecorder) {

  private val logger    = LoggerFactory.getLogger(getClass)
  private var cpgconfig = JavaConfig()
  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    ruleCache: RuleCache,
    sourceRepoLocation: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) => {
        try {
          statsRecorder.initiateNewStage("Parser passes")
          new HTMLParserPass(cpg, sourceRepoLocation, ruleCache, privadoInputConfig = ScanProcessor.config.copy())
            .createAndApply()
          new SQLParser(cpg, sourceRepoLocation, ruleCache).createAndApply()
          new DBTParserPass(cpg, sourceRepoLocation, ruleCache).createAndApply()
          statsRecorder.endLastStage()
          // Run tagger
          statsRecorder.initiateNewStage("Tagging")
          val taggerCache = new TaggerCache
          cpg.runTagger(ruleCache, taggerCache)
          statsRecorder.endLastStage()
          statsRecorder.initiateNewStage("Finding source to sink flow")
          val dataflowMap =
            Dataflow(cpg, statsRecorder).dataflow(ScanProcessor.config, ruleCache, dataFlowCache, auditCache, appCache)
          statsRecorder.endLastStage()
          statsRecorder.justLogMessage(s"Processed final flows - ${dataFlowCache.getDataflowAfterDedup.size}")
          statsRecorder.initiateNewStage("Brewing result...")
          MetricHandler.setScanStatus(true)
          val errorMsg = new ListBuffer[String]()
          // Exporting Results
          JSONExporter.fileExport(
            cpg,
            outputFileName,
            sourceRepoLocation,
            dataflowMap,
            ruleCache,
            taggerCache,
            dataFlowCache.getDataflowAfterDedup,
            ScanProcessor.config,
            List(),
            s3DatabaseDetailsCache,
            appCache,
            propertyFilterCache
          ) match {
            case Left(err) =>
              MetricHandler.otherErrorsOrWarnings.addOne(err)
              errorMsg += err
            case Right(_) =>
              statsRecorder.justLogMessage(
                s"Successfully exported output to '${appCache.localScanPath}/$outputDirectoryName' folder..."
              )
              logger.debug(
                s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
              )
          }

          // Check if any of the export failed
          if (errorMsg.toList.isEmpty)
            Right(())
          else
            Left(errorMsg.toList.mkString("\n"))
        } finally {
          cpg.close()
          import java.io.File
          val cpgFile = new File(cpgconfig.outputPath)
          statsRecorder.justLogMessage(
            s"Binary file size -- ${cpgFile.length()} in Bytes - ${cpgFile.length() * 0.000001} MB\n\n\n"
          )
        }
      }

      case Failure(exception) => {
        logger.error("Error while parsing the source code!")
        logger.debug("Error : ", exception)
        MetricHandler.setScanStatus(false)
        Left("Error while parsing the source code: " + exception.toString)
      }
    }
  }

  /** Create cpg using default pass
    *
    * @param sourceRepoLocation
    * @param lang
    * @return
    */
  def createDefaultCpg(
    ruleCache: RuleCache,
    sourceRepoLocation: String,
    dataFlowCache: DataFlowCache,
    auditCache: AuditCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache,
    propertyFilterCache: PropertyFilterCache
  ): Either[String, Unit] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using default pass")

    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    cpgconfig = JavaConfig(fetchDependencies = !config.skipDownloadDependencies)
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutputPath)

    val xtocpg = withNewEmptyCpg(cpgOutputPath, cpgconfig: JavaConfig) { (cpg, config) => {} }

    val msg =
      processCPG(
        xtocpg,
        ruleCache,
        sourceRepoLocation,
        dataFlowCache,
        auditCache,
        s3DatabaseDetailsCache,
        appCache,
        propertyFilterCache = propertyFilterCache
      )
    msg
  }
}
