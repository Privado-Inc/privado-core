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
import ai.privado.cache.{AppCache, DataFlowCache, Environment, RuleCache, S3DatabaseDetailsCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{outputDirectoryName, value}
import ai.privado.model.exporter.{
  AndroidPermissionModel,
  CollectionModel,
  DataFlowSourceIntermediateModel,
  DataFlowSubCategoryModel,
  SinkModel,
  SinkProcessingModel,
  SourceModel,
  SourceProcessingModel,
  ViolationModel
}
import ai.privado.model.{Constants, DataFlowPathModel, PolicyThreatType}
import ai.privado.model.exporter.SourceEncoderDecoder.*
import ai.privado.model.exporter.DataFlowEncoderDecoder.*
import ai.privado.model.exporter.ViolationEncoderDecoder.*
import ai.privado.model.exporter.CollectionEncoderDecoder.*
import ai.privado.model.exporter.AndroidPermissionsEncoderDecoder.*
import ai.privado.model.exporter.SinkEncoderDecoder.*
import ai.privado.script.ExternalScalaScriptRunner
import better.files.File
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
    dataFlowModel: List[DataFlowPathModel],
    privadoInput: PrivadoInput,
    monolithPrivadoJsonPaths: List[String] = List(),
    s3DatabaseDetailsCache: S3DatabaseDetailsCache,
    appCache: AppCache
  ): Either[String, Map[String, Json]] = {

    try {
      val (
        output: mutable.LinkedHashMap[String, Json],
        sources: List[SourceModel],
        sinks: List[SinkModel],
        processing: List[SourceProcessingModel],
        dataflowsOutput: mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]],
        finalCollections: List[CollectionModel],
        complianceViolationSize: Int
      ) = ExporterUtility.generateIndividualComponent(
        cpg,
        outputFileName,
        repoPath,
        dataflows,
        ruleCache,
        taggerCache,
        dataFlowModel,
        privadoInput,
        s3DatabaseDetailsCache,
        appCache = appCache
      )

      // Add the privado json path of each monolith repository item
      output.addOne(Constants.monolithJsonPath -> monolithPrivadoJsonPaths.asJson)

      ConsoleExporter.exportConsoleSummary(
        dataflowsOutput,
        sources,
        sinks,
        processing,
        finalCollections,
        complianceViolationSize
      )

      logger.debug(
        s"Total False positive flows removed : \n" +
          s"Total flows before FP: ${appCache.totalFlowFromReachableBy}\n" +
          s"Total flows after this filtering: ${appCache.totalFlowAfterThisFiltering}\n" +
          s"FP by overlapping Data element : ${appCache.fpByOverlappingDE}\n" +
          s"Total flows after complete computation : ${dataFlowModel.size}"
      )

      logger.debug(s"Final statistics for FP : ${appCache.fpMap}, for total ${appCache.totalMap}")

      // Post export Trigger
      ExternalScalaScriptRunner
        .postExportTrigger(cpg, ruleCache, output)

      val jsonFile = ExporterUtility.writeJsonToFile(cpg, outputFileName, repoPath, ruleCache, output.toMap)

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
      MetricHandler.metricsData("fileSizeInKB") = Json.fromLong(jsonFile.size / 1024)
      Right(output.toMap)

    } catch {
      case ex: Exception =>
        println("Failed to export output")
        logger.debug(ex.getStackTrace.mkString("\n"))
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
        logger.debug(ex.getStackTrace.mkString("\n"))
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
        logger.debug(ex.getStackTrace.mkString("\n"))
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
        logger.debug(ex.getStackTrace.mkString("\n"))
        logger.debug("Failed to export unresolved output", ex)
        Left(ex.toString)
    }
  }
}
