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
import ai.privado.model.Constants.outputDirectoryName
import ai.privado.model.exporter.SourceEncoderDecoder._
import ai.privado.model.exporter.DataFlowEncoderDecoder._
import ai.privado.model.exporter.ViolationEncoderDecoder._
import ai.privado.model.exporter.CollectionEncoderDecoder._
import ai.privado.model.exporter.{DataFlowSubCategoryModel, ViolationModel}
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
import java.net.URL
import Console.{BLUE, BOLD, CYAN, GREEN, RED, RESET, YELLOW, WHITE, MAGENTA}


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

      val dataflowsOutput = mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]()
      sinkSubCategories.foreach(sinkSubTypeEntry => {
        dataflowsOutput.addOne(
          sinkSubTypeEntry._1 -> dataflowExporter.getFlowByType(sinkSubTypeEntry._1, sinkSubTypeEntry._2.toSet).toList
        )
      })

      // Parse the Dataflows
      val sourceNameIdMap = mutable.HashMap[String, String]()
      val leakageSourceMap = mutable.HashMap[String, Int]()
      val storageSourceMap = mutable.HashMap[String, mutable.Set[String]]()
      val thirdPartySourceMap = mutable.HashMap[String, mutable.Set[String]]()
      val internalAPIsSourceMap = mutable.HashMap[String, mutable.Set[String]]()
      var uniqueThirdParties = mutable.Set[String]()

      // SourceId - Name Map
      sourceExporter.getSources.foreach(source => {
        sourceNameIdMap.addOne(
          source.id.replaceAll("\"", "") -> source.name.replaceAll("\"", "")
        )
      })

      // Leakage Number - SourceId Map
      dataflowsOutput("leakages").foreach(leakage => {
        leakageSourceMap.addOne(leakage.sourceId -> leakage.sinks.size )
      })

      // Storages - SourceId Map
      dataflowsOutput("storages").foreach(storage => {
        val storages = mutable.Set[String]()
        storage.sinks.foreach(sink => {
          storages.addOne(sink.name)
        })
        storageSourceMap.addOne(storage.sourceId -> storages)
      })

      // Third Parties - SourceId Map
      dataflowsOutput("third_parties").foreach(thirdParty => {
        val thirdParties = mutable.Set[String]()
        thirdParty.sinks.foreach(sink => {
          if (sink.apiUrl.size > 0) {
            sink.apiUrl.foreach(urlString => {
              val url = new URL("https://" + urlString.replaceAll("https://", "").trim)
              thirdParties.addOne(url.getHost.replaceAll("www.", ""))
              uniqueThirdParties.addOne(url.getHost.replaceAll("www.", ""))
            })
          } else {
            thirdParties.addOne(sink.name)
            uniqueThirdParties.addOne((sink.name))
          }
        })
        thirdPartySourceMap.addOne(thirdParty.sourceId -> thirdParties)
      })

      // Internal APIs - SourceId Map
      dataflowsOutput("internal_apis").foreach(internalAPI => {
        val internalAPIs = mutable.Set[String]()
        internalAPI.sinks.foreach(sink => {
          if (sink.apiUrl.size > 0) {
            sink.apiUrl.foreach(urlString => {
              val url = new URL("https://" + urlString.replaceAll("https://", "").trim)
              internalAPIs.addOne(url.getHost.replaceAll("www.", ""))
            })
          }
        })
        internalAPIsSourceMap.addOne(internalAPI.sourceId -> internalAPIs)
      })

      output.addOne(Constants.dataFlow -> dataflowsOutput.asJson)
      logger.info("Completed Sink Exporting")

      output.addOne(Constants.collections -> collectionExporter.getCollections.asJson)
      logger.info("Completed Collections Exporting")

      val violations = policyAndThreatExporter.getViolations(repoPath)
      output.addOne("violations" -> violations.asJson)
      MetricHandler.metricsData("policyViolations") = violations.size.asJson
      violations.foreach(violation => {
        MetricHandler.internalPoliciesOrThreatsMatched.addOne(violation.policyId)
      })

      logger.info("Completed exporting policy violations")
      File(s"$repoPath/$outputDirectoryName").createDirectoryIfNotExists()
      val f = File(s"$repoPath/$outputDirectoryName/$outputFileName")
      f.write(output.asJson.toString())
      logger.info("Shutting down Exporter engine")
      logger.info("Scanning Completed...")

      println("\n---------------------------------------------------------")
      println("SUMMARY")
      println("-----------------------------------------------------------")
      println("\nPrivado discovers data elements that are being collected, processed, or shared in the code.\n")
      println(s"DATA ELEMENTS  |  ${sourceNameIdMap.size} |")
      println(s"THIRD PARTY    |  ${uniqueThirdParties.size} |")
      println(s"ISSUES         |  ${violations.size} |")
      println("\n---------------------------------------------------------")
      println(s"${sourceNameIdMap.size} DATA ELEMENTS")
      println("Privado discovers data elements that are being collected, processed, or shared in the code.")

      val sourceSummaryMap = mutable.HashMap[String, mutable.HashMap[String, Any]]()
      var count = 0;
      sourceNameIdMap.foreachEntry((sourceId, sourceName) => {
        count = count + 1
        if (count <= 10) {
          val dataflowSummary = mutable.HashMap[String, Any]()
          Console.println(s"\n${RESET}${WHITE}${BOLD}${count}. ${sourceName.toUpperCase()}${RESET}")

          if (thirdPartySourceMap.contains(sourceId)) {
            Console.println(s"\t${RESET}${YELLOW}Third Parties${RESET}    ->  ${thirdPartySourceMap(sourceId).toList.mkString(", ")}")
            dataflowSummary.addOne("third_parties" -> thirdPartySourceMap(sourceId))
          }
          if (storageSourceMap.contains(sourceId)) {
            Console.println(s"\t${RESET}${BLUE}Storage${RESET}         ->  ${storageSourceMap(sourceId).toList.mkString(", ")}")
            dataflowSummary.addOne("storages" -> storageSourceMap(sourceId))
          }
          if (internalAPIsSourceMap.contains(sourceId)) {
            Console.println(s"\t${RESET}${CYAN}Internal APIs${RESET}      ->  ${internalAPIsSourceMap(sourceId).toList.mkString(", ")}")
            dataflowSummary.addOne("internal_apis" -> internalAPIsSourceMap(sourceId))
          }
          if (leakageSourceMap.contains(sourceId)) {
            Console.println(s"\t${RESET}${RED}Leakage${RESET}         ->  ${leakageSourceMap(sourceId)}")
            dataflowSummary.addOne("leakages" -> leakageSourceMap(sourceId))
          }
          if (dataflowSummary.size == 0) {
            Console.println(s"\t${RESET}${MAGENTA}Processing${RESET}")
          }
          sourceSummaryMap.addOne(sourceName -> dataflowSummary)
        }
      })

      println("")
      if (sourceNameIdMap.size > 10) {
        println(s"--------- ${sourceNameIdMap.size - 10} more results ---------\n")
      }

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
      case ex: Exception =>
        println("Failed to export output")
        logger.debug("Failed to export output", ex)
        Left(ex.toString)
    }
  }

}
