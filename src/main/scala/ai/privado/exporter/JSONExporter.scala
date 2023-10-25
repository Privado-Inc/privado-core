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

package ai.privado.exporter

import ai.privado.audit.AuditReportEntryPoint.DataElementDiscoveryAudit
import ai.privado.cache.{AppCache, DataFlowCache, Environment, RuleCache, TaggerCache}
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{outputDirectoryName, value}
import ai.privado.model.exporter.{
  CollectionModel,
  DataFlowSourceIntermediateModel,
  DataFlowSubCategoryModel,
  SinkModel,
  SinkProcessingModel,
  SourceModel,
  SourceProcessingModel,
  ViolationModel
}
import ai.privado.model.{Constants, PolicyThreatType}
import ai.privado.model.exporter.SourceEncoderDecoder.*
import ai.privado.model.exporter.DataFlowEncoderDecoder.*
import ai.privado.model.exporter.ViolationEncoderDecoder.*
import ai.privado.model.exporter.CollectionEncoderDecoder.*
import ai.privado.model.exporter.SinkEncoderDecoder.*
import ai.privado.script.ExternalScalaScriptRunner
import better.files.File
import privado_core.BuildInfo
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import org.apache.commons.io.FileUtils
import org.slf4j.LoggerFactory

import java.math.BigInteger
import java.util.Calendar
import scala.collection.mutable
import scala.concurrent.*
import ExecutionContext.Implicits.global
import duration.*
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import io.shiftleft.semanticcpg.language.*

object JSONExporter {

  private val logger = LoggerFactory.getLogger(getClass)

  def fileExport(
    cpg: Cpg,
    outputFileName: String,
    repoPath: String,
    dataflows: Map[String, Path],
    ruleCache: RuleCache,
    taggerCache: TaggerCache = new TaggerCache(),
    dataFlowCache: DataFlowCache
  ): Either[String, Unit] = {
    logger.info("Initiated exporter engine")
    val sourceExporter          = new SourceExporter(cpg, ruleCache)
    val sinkExporter            = new SinkExporter(cpg, ruleCache)
    val dataflowExporter        = new DataflowExporter(cpg, dataflows, taggerCache, dataFlowCache)
    val collectionExporter      = new CollectionExporter(cpg, ruleCache)
    val probableSinkExporter    = new ProbableSinkExporter(cpg, ruleCache, repoPath)
    val policyAndThreatExporter = new PolicyAndThreatExporter(cpg, ruleCache, dataflows, taggerCache, dataFlowCache)
    val output                  = mutable.LinkedHashMap[String, Json]()
    try {

      output.addOne(Constants.coreVersion -> Environment.privadoVersionCore.asJson)
      output.addOne(Constants.cliVersion  -> Environment.privadoVersionCli.getOrElse(Constants.notDetected).asJson)
      output.addOne(Constants.mainVersion -> AppCache.privadoVersionMain.asJson)
      output.addOne(Constants.privadoLanguageEngineVersion -> BuildInfo.joernVersion.asJson)
      output.addOne(Constants.createdAt                    -> Calendar.getInstance().getTimeInMillis.asJson)
      output.addOne(Constants.repoName                     -> AppCache.repoName.asJson)
      output.addOne(Constants.language                     -> AppCache.repoLanguage.toString.asJson)
      output.addOne(Constants.gitMetadata                  -> GitMetaDataExporter.getMetaData(repoPath).asJson)
      output.addOne(Constants.localScanPath                -> AppCache.localScanPath.asJson)
      output.addOne(Constants.probableSinks                -> probableSinkExporter.getProbableSinks.asJson)

      // Future creates a thread and starts resolving the function call asynchronously
      val sources = Future {
        val _sources = Try(sourceExporter.getSources).getOrElse(List[SourceModel]())
        output.addOne(Constants.sources -> _sources.asJson)
        _sources
      }
      val processing = Future {
        val _processing = Try(sourceExporter.getProcessing).getOrElse(List[SourceProcessingModel]())
        output.addOne(Constants.processing -> _processing.asJson)
        _processing
      }
      val sinks = Future {
        val _sinks = Try(sinkExporter.getSinks).getOrElse(List[SinkModel]())
        output.addOne(Constants.sinks -> _sinks.asJson)
        _sinks
      }
      val processingSinks = Future {
        val _processingSinks = Try(sinkExporter.getProcessing).getOrElse(List[SinkProcessingModel]())
        output.addOne(Constants.sinkProcessing -> _processingSinks.asJson)
        _processingSinks
      }
      val collections = Future {
        val _collections = Try(collectionExporter.getCollections).getOrElse(List[CollectionModel]())
        output.addOne(Constants.collections -> _collections.asJson)
        _collections
      }

      val violationResult = Try(policyAndThreatExporter.getViolations(repoPath)).getOrElse(List[ViolationModel]())
      output.addOne(Constants.violations -> violationResult.asJson)

      val sinkSubCategories = mutable.HashMap[String, mutable.Set[String]]()
      ruleCache.getRule.sinks.foreach(sinkRule => {
        if (!sinkSubCategories.contains(sinkRule.catLevelTwo))
          sinkSubCategories.addOne(sinkRule.catLevelTwo -> mutable.Set())
        sinkSubCategories(sinkRule.catLevelTwo).add(sinkRule.nodeType.toString)
      })

      val dataflowsOutput = mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]()
      sinkSubCategories.foreach(sinkSubTypeEntry => {
        dataflowsOutput.addOne(
          sinkSubTypeEntry._1 -> dataflowExporter
            .getFlowByType(sinkSubTypeEntry._1, sinkSubTypeEntry._2.toSet, ruleCache, dataFlowCache)
            .toList
        )
      })

      output.addOne(Constants.dataFlow -> dataflowsOutput.asJson)
      logger.info("Completed Sink Exporting")

      logger.info("Completed Collections Exporting")

      MetricHandler.metricsData("policyViolations") = violationResult.size.asJson
      violationResult.foreach(violation => {
        MetricHandler.internalPoliciesOrThreatsMatched.addOne(violation.policyId)
      })

      //  Compliance Violations
      val complianceViolations = violationResult.filter(violation =>
        violation.policyDetails match {
          case Some(policyDetail) => policyDetail.policyType.equals(PolicyThreatType.COMPLIANCE.toString)
          case None               => false
        }
      )

      // We need to wait till this get completed before moving ahead to export the result
      Await.result(processingSinks, Duration.Inf)

      ConsoleExporter.exportConsoleSummary(
        dataflowsOutput,
        Await.result(sources, Duration.Inf),
        Await.result(sinks, Duration.Inf),
        Await.result(processing, Duration.Inf),
        Await.result(collections, Duration.Inf),
        complianceViolations.size
      )

      logger.debug(
        s"Total False positive flows removed : \n" +
          s"Total flows before FP: ${AppCache.totalFlowFromReachableBy}\n" +
          s"Total flows after this filtering: ${AppCache.totalFlowAfterThisFiltering}\n" +
          s"FP by overlapping Data element : ${AppCache.fpByOverlappingDE}\n" +
          s"Total flows after complete computation : ${dataFlowCache.getDataflow.size}"
      )

      logger.debug(s"Final statistics for FP : ${AppCache.fpMap}, for total ${AppCache.totalMap}")

      logger.info("Completed exporting policy violations")
      val outputDirectory = File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
      val f               = File(s"$repoPath/$outputDirectoryName/$outputFileName")

      // Post export Trigger
      ExternalScalaScriptRunner
        .postExportTrigger(cpg, ruleCache, output)

      f.write(output.asJson.toString())
      logger.info("Shutting down Exporter engine")
      logger.info("Scanning Completed...")

      try {
        MetricHandler.metricsData("repoSizeInKB") = Json.fromBigInt(
          FileUtils.sizeOfDirectoryAsBigInteger(new java.io.File(repoPath)).divide(BigInteger.valueOf(1024))
        )
      } catch {
        case e: Exception =>
          logger.error("Error fetching the size of repo")
          logger.debug("Error in getting size of repo ", e)
      }
      MetricHandler.metricsData("fileSizeInKB") = Json.fromLong(f.size / 1024)
      Right(())

    } catch {
      case ex: Exception =>
        println("Failed to export output")
        logger.debug("Failed to export output", ex)
        Left(ex.toString)
    }
  }

  def IntermediateFileExport(
    outputFileName: String,
    repoPath: String,
    dataflows: List[DataFlowSourceIntermediateModel]
  ): Either[String, Unit] = {
    logger.info("Initiated the Intermediate exporter engine")
    val output = mutable.LinkedHashMap[String, Json]()
    try {
      output.addOne(Constants.dataFlow -> dataflows.asJson)
      logger.info("Completed Intermediate Sink Exporting")

      val outputDir = File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
      val f         = File(s"$repoPath/$outputDirectoryName/$outputFileName")
      f.write(output.asJson.toString())
      logger.info("Shutting down Intermediate Exporter engine")
      Right(())

    } catch {
      case ex: Exception =>
        println("Failed to export intermediate output")
        logger.debug("Failed to export intermediate output", ex)
        Left(ex.toString)
    }
  }

  def dataElementDiscoveryAuditFileExport(
    outputFileName: String,
    repoPath: String,
    dataElementDiscoveryData: List[DataElementDiscoveryAudit]
  ): Either[String, Unit] = {
    logger.info("Initiated the Intermediate exporter engine")
    val output = dataElementDiscoveryData.asJson
    try {
      logger.info("Completed Intermediate Data-Element Discovery Exporting")

      val f = File(s"$repoPath/$outputDirectoryName/$outputFileName")
      f.write(output.asJson.toString())
      logger.info("Shutting down Intermediate Exporter engine")
      Right(())

    } catch {
      case ex: Exception =>
        println("Failed to export intermediate output")
        logger.debug("Failed to export intermediate output", ex)
        Left(ex.toString)
    }
  }

  def UnresolvedFlowFileExport(
    outputFileName: String,
    repoPath: String,
    dataflows: List[DataFlowSourceIntermediateModel]
  ): Either[String, Unit] = {
    logger.info("Initiated the Unresolved flow exporter engine")
    val output = mutable.LinkedHashMap[String, Json]()
    try {
      output.addOne(Constants.dataFlow -> dataflows.asJson)
      logger.info("Completed Unresolved Flow Exporting")

      val outputDir = File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
      val f         = File(s"$repoPath/$outputDirectoryName/$outputFileName")
      f.write(output.asJson.toString())
      logger.info("Shutting down Unresolved flow Exporter engine")
      Right(())
    } catch {
      case ex: Exception =>
        println("Failed to export unresolved output")
        logger.debug("Failed to export unresolved output", ex)
        Left(ex.toString)
    }
  }
}
