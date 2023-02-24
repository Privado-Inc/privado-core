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

import ai.privado.cache.{AppCache, DataFlowCache, Environment, RuleCache}
import ai.privado.entrypoint.ScanProcessor
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{outputDirectoryName, sinkProcessing}
import ai.privado.model.exporter.{
  DataFlowSubCategoryModel,
  SinkModel,
  SinkProcessingModel,
  SourceModel,
  SourceProcessingModel
}
import ai.privado.model.{CatLevelOne, Constants, DataFlowPathModel, InternalTag, PolicyThreatType}
import ai.privado.model.Constants.outputDirectoryName
import ai.privado.model.exporter.{
  DataFlowPathIntermediateModel,
  DataFlowSourceIntermediateModel,
  DataFlowSubCategoryModel,
  DataFlowSubCategoryPathExcerptModel
}
import ai.privado.model.exporter.SourceEncoderDecoder._
import ai.privado.model.exporter.DataFlowEncoderDecoder._
import ai.privado.model.exporter.ViolationEncoderDecoder._
import ai.privado.model.exporter.CollectionEncoderDecoder._
import ai.privado.model.exporter.SinkEncoderDecoder._
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
import scala.concurrent._
import ExecutionContext.Implicits.global
import duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}
import io.shiftleft.semanticcpg.language._

object JSONExporter {

  private val logger = LoggerFactory.getLogger(getClass)

  def fileExport(
    cpg: Cpg,
    outputFileName: String,
    repoPath: String,
    dataflows: Map[String, Path]
  ): Either[String, Unit] = {
    logger.info("Initiated exporter engine")
    val sourceExporter          = new SourceExporter(cpg)
    val sinkExporter            = new SinkExporter(cpg)
    val dataflowExporter        = new DataflowExporter(cpg, dataflows)
    val collectionExporter      = new CollectionExporter(cpg)
    val policyAndThreatExporter = new PolicyAndThreatExporter(cpg, dataflows)
    val output                  = mutable.LinkedHashMap[String, Json]()
    try {

      output.addOne(Constants.coreVersion -> Environment.privadoVersionCore.asJson)
      output.addOne(Constants.cliVersion  -> Environment.privadoVersionCli.getOrElse(Constants.notDetected).asJson)
      output.addOne(Constants.mainVersion -> AppCache.privadoVersionMain.asJson)
      output.addOne(Constants.privadoLanguageEngineVersion -> BuildInfo.joernVersion.asJson)
      output.addOne(Constants.createdAt                    -> Calendar.getInstance().getTimeInMillis.asJson)
      output.addOne(Constants.repoName                     -> AppCache.repoName.asJson)
      output.addOne(Constants.gitMetadata                  -> GitMetaDataExporter.getMetaData(repoPath).asJson)
      output.addOne(Constants.localScanPath                -> AppCache.localScanPath.asJson)

      val sources = Future(
        sourceExporter.getSources
      ) // Future creates a thread and starts resolving the function call asynchronously
      val processing      = Future(sourceExporter.getProcessing)
      val sinks           = Future(sinkExporter.getSinks)
      val processingSinks = Future(sinkExporter.getProcessing)
      val probableSinks   = Future(sinkExporter.getProbableSinks)
      val collections     = Future(collectionExporter.getCollections)
      val violations      = Future(policyAndThreatExporter.getViolations(repoPath))

      // Called when the asynchronous call is completed
      sources.onComplete({
        case Success(value) => {
          output.addOne(Constants.sources -> value.asJson)
          logger.info("Completed sources export.")
        }
        case Failure(e) => {
          println(e)
        }
      })

      sinks.onComplete({
        case Success(value) => {
          output.addOne(Constants.sinks -> value.asJson)
          logger.info("Completed sinks export.")
        }
        case Failure(e) => {
          println(e)
        }
      })

      processing.onComplete({
        case Success(value) => {
          output.addOne(Constants.processing -> value.asJson)
          logger.info("Completed processing export.")

        }
        case Failure(e) => {
          println(e)
        }
      })

      collections.onComplete({
        case Success(value) => {
          output.addOne(Constants.collections -> value.asJson)
          logger.info("Completed collections export.")
        }
        case Failure(e) => {
          println(e)
        }
      })

      probableSinks.onComplete({
        case Success(value) => {
          output.addOne(Constants.probableSinks -> value.asJson)
          logger.info("Completed probable sinks export.")
        }
        case Failure(e) => {
          println(e)
        }
      })

      processingSinks.onComplete({
        case Success(value) => {
          output.addOne(Constants.sinkProcessing -> value.asJson)
          logger.info("Completed sinks export.")
        }
        case Failure(e) => {
          println(e)
        }
      })

      violations.onComplete({
        case Success(value) => {
          output.addOne("violations" -> value.asJson)
          logger.info("Completed violation export.")
        }
        case Failure(e) => {
          println(e)
        }
      })

      val sinkSubCategories = mutable.HashMap[String, mutable.Set[String]]()
      RuleCache.getRule.sinks.foreach(sinkRule => {
        if (!sinkSubCategories.contains(sinkRule.catLevelTwo))
          sinkSubCategories.addOne(sinkRule.catLevelTwo -> mutable.Set())
        sinkSubCategories(sinkRule.catLevelTwo).add(sinkRule.nodeType.toString)
      })

      val dataflowsOutput = mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]()
      sinkSubCategories.foreach(sinkSubTypeEntry => {
        dataflowsOutput.addOne(
          sinkSubTypeEntry._1 -> dataflowExporter.getFlowByType(sinkSubTypeEntry._1, sinkSubTypeEntry._2.toSet).toList
        )
      })

      output.addOne(Constants.dataFlow -> dataflowsOutput.asJson)
      logger.info("Completed Sink Exporting")

      logger.info("Completed Collections Exporting")

      val violationResult = Await.result(violations, 10 seconds);

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
      logger.debug("------------ Sink Skip List ---------------")
      val skipRules = RuleCache.getRule.sinkSkipList.map(sinkSkipRule => sinkSkipRule.combinedRulePattern)
      logger.debug(s"$skipRules")
      logger.debug("------------ Probable Sink Dependencies ---------------")
      logger.debug(s"$probableSinks")

      ConsoleExporter.exportConsoleSummary(
        dataflowsOutput,
        Await.result(sources, 10 seconds),
        Await.result(sinks, 10 seconds),
        Await.result(processing, 10 seconds),
        Await.result(collections, 10 seconds),
        complianceViolations.size
      )

      logger.debug(
        s"Total False positive flows removed : \n" +
          s"Total flows before FP: ${AppCache.totalFlowFromReachableBy}\n" +
          s"Total flows after this filtering: ${AppCache.totalFlowAfterThisFiltering}\n" +
          s"FP by overlapping Data element : ${AppCache.fpByOverlappingDE}\n" +
          s"Total flows after complete computation : ${DataFlowCache.getDataflow.size}"
      )

      logger.debug(s"Final statistics for FP : ${AppCache.fpMap}, for total ${AppCache.totalMap}")

      logger.info("Completed exporting policy violations")
      val outputDirectory = File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
      val f               = File(s"$repoPath/$outputDirectoryName/$outputFileName")
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
    logger.info("Initialed the Intermediate exporter engine")
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

}
