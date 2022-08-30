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
 */

package ai.privado.exporter

import ai.privado.cache.{AppCache, Environment, RuleCache}
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants
import ai.privado.model.Constants.{outputDirectoryName, outputFileName}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.circe._
import io.circe.syntax._
import io.joern.dataflowengineoss.language.Path
import org.apache.commons.io.FileUtils

import java.util.Calendar
import scala.collection.mutable
import better.files.File
import org.slf4j.LoggerFactory

import java.math.BigInteger

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
    val dataflowExporter        = new DataflowExporter(cpg, dataflows)
    val collectionExporter      = new CollectionExporter(cpg)
    val policyAndThreatExporter = new PolicyAndThreatExporter(cpg, dataflows)
    val output                  = mutable.LinkedHashMap[String, Json]()
    try {
      output.addOne(Constants.coreVersion   -> Environment.privadoVersionCore.asJson)
      output.addOne(Constants.cliVersion    -> Environment.privadoVersionCli.getOrElse(Constants.notDetected).asJson)
      output.addOne(Constants.mainVersion   -> AppCache.privadoVersionMain.asJson)
      output.addOne(Constants.createdAt     -> Calendar.getInstance().getTimeInMillis.asJson)
      output.addOne(Constants.repoName      -> AppCache.repoName.asJson)
      output.addOne(Constants.gitMetadata   -> GitMetaDataExporter.getMetaData(repoPath).asJson)
      output.addOne(Constants.localScanPath -> AppCache.localScanPath.asJson)
      output.addOne(Constants.sources       -> sourceExporter.getSources.asJson)
      output.addOne(Constants.processing    -> sourceExporter.getProcessing.asJson)
      logger.info("Completed Source Exporting")

      val sinkSubCategories = mutable.HashMap[String, mutable.Set[String]]()
      RuleCache.getRule.sinks.foreach(sinkRule => {
        if (!sinkSubCategories.contains(sinkRule.catLevelTwo))
          sinkSubCategories.addOne(sinkRule.catLevelTwo -> mutable.Set())
        sinkSubCategories(sinkRule.catLevelTwo).add(sinkRule.nodeType.toString)
      })

      val dataflowsOutput = mutable.LinkedHashMap[String, Json]()
      sinkSubCategories.foreach(sinkSubTypeEntry => {
        dataflowsOutput.addOne(
          sinkSubTypeEntry._1 -> dataflowExporter.getFlowByType(sinkSubTypeEntry._1, sinkSubTypeEntry._2.toSet).asJson
        )
      })

      output.addOne(Constants.dataFlow -> dataflowsOutput.asJson)
      logger.info("Completed Sink Exporting")

      output.addOne(Constants.collections -> collectionExporter.getCollections.asJson)
      logger.info("Completed Collections Exporting")

      val violations = policyAndThreatExporter.getViolations(repoPath)
      output.addOne("violations" -> violations.asJson)
      MetricHandler.metricsData("policyViolations") = Json.fromInt(violations.size)
      violations.foreach(mapEntry => {
        mapEntry("policyId").asString match {
          case Some(value) =>
            MetricHandler.internalPoliciesOrThreatsMatched.addOne(value)
          case _ => ()
        }
      })

      logger.info("Completed exporting policy violations")
      File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
      val f = File(s"$repoPath/$outputDirectoryName/$outputFileName")
      f.write(output.asJson.toString())
      logger.info("Shutting down Exporter engine")
      logger.info("Scanning Completed...")
      try {
        MetricHandler.metricsData("repoSize (in KB)") = Json.fromBigInt(
          FileUtils.sizeOfDirectoryAsBigInteger(new java.io.File(repoPath)).divide(BigInteger.valueOf(1024))
        )
      } catch {
        case e: Exception =>
          logger.error("Error fetching the size of repo")
          logger.debug("Error in getting size of repo ", e)
      }
      MetricHandler.metricsData("fileSize (in KB)") = Json.fromLong(f.size / 1024)
      Right(())

    } catch {
      case ex: Exception => {
        println("Failed to export output")
        logger.debug("Failed to export output", ex)
        Left(ex.toString)
      }
    }
  }

}
