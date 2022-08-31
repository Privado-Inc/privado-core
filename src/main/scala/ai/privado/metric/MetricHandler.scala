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

package ai.privado.metric
import ai.privado.cache.{AppCache, Environment}
import ai.privado.exporter.GitMetaDataExporter
import ai.privado.utility.Utilities
import io.circe.Json
import org.slf4j.LoggerFactory

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import io.circe.syntax._
import Console.{BLUE, BOLD, CYAN, GREEN, RED, RESET, YELLOW, WHITE, MAGENTA}

object MetricHandler {

  private val logger                   = LoggerFactory.getLogger(this.getClass)
  val metricsData                      = mutable.HashMap[String, Json]()
  val scanProcessErrors                = ArrayBuffer[String]()
  val totalRulesMatched                = mutable.Set[String]()
  val internalRulesMatched             = mutable.Set[String]()
  val flowCategoryData                 = mutable.HashMap[String, Int]()
  val internalPoliciesOrThreatsMatched = mutable.Set[String]()

  metricsData("privadoCoreVersion") = Environment.privadoVersionCore.asJson
  metricsData("privadoCoreCommand") = Json.Null
  val gitMetaData = GitMetaDataExporter.getMetaData(AppCache.localScanPath)
  metricsData("hashedRepoIdentifier") = Json.fromString(Utilities.getSHA256Hash(gitMetaData.size match {
    case 0 => AppCache.repoName
    case _ => gitMetaData("remoteUrl")
  }))

  def timeMetric[R](block: => R, call: String): R = {
    val startTime = System.nanoTime()
    val result    = block
    val endTime   = System.nanoTime()
    metricsData(s"timeTakenTo$call") =
      Json.fromLong(TimeUnit.SECONDS.convert((endTime - startTime), TimeUnit.NANOSECONDS))
    result
  }

  def compileAndSend() = {
    metricsData("internalRuleIdsMatch") = Json.fromValues(internalRulesMatched.map(key => Json.fromString(key)))
    metricsData("scanProcessErrors") = scanProcessErrors.asJson
    metricsData("flowCategoryData") = flowCategoryData.asJson
    metricsData("noOfRulesMatch") = Json.fromInt(totalRulesMatched.size)
    metricsData("internalPoliciesIdsMatch") =
      Json.fromValues(internalPoliciesOrThreatsMatched.map(key => Json.fromString(key)))
    printSummary()
    sendDataToServer()
  }

  def printSummary() = {
    println(s"\n----------------------------------------------------------------------------------------------")
    println(s"SUMMARY")
    println(s"----------------------------------------------------------------------------------------------\n")
    println(s"Privado discovers data elements that are being collected, processed, or shared in the code.\n")
    Console.println(s"${RESET}${BLUE}${BOLD}DATA ELEMENTS${RESET}  |  ${metricsData("sources")}")
    println(s"----------------------------------------------------------------------------------------------\n")
    println(s"Sinks are the destinations where personal data is being sent in the code. They are further categorised into storages, leakages, third parties, and internal apis.\n")
    Console.println(s"${RESET}${GREEN}Third Parties${RESET}  |  ${flowCategoryData("third_parties")}")
    Console.println(s"${RESET}${CYAN}Storage${RESET}        |  ${flowCategoryData("storages")}")
    Console.println(s"${RESET}${WHITE}Internal API${RESET}   |  ${flowCategoryData("internal_apis")}")
    Console.println(s"${RESET}${MAGENTA}Data Leakages${RESET}  |  ${flowCategoryData("leakages")}")
    println(s"----------------------------------------------------------------------------------------------\n")
    println(s"Collections are the points in the code where personal data enters either via direct user input or via other API/service pushing it.\n")
    Console.println(s"${RESET}${RED}Collection${RESET}     |  ${metricsData("collections")}")
    println(s"----------------------------------------------------------------------------------------------\n")
    println(s"Privado creates issues based on policy rules and CWE vulnerabilities. Issues can be used to monitor and govern the usage of personal data.\n")
    Console.println(s"${RESET}${RED}Issues${RESET}         |  ${Json.fromInt(internalPoliciesOrThreatsMatched.size)}")
    println(s"----------------------------------------------------------------------------------------------\n")
  }

  def sendDataToServer() = {
    // Check if metrics are disabled
    var metricsEndPoint = "https://cli.privado.ai/api/event?version=2"
    Environment.metricsEnabled match {
      case Some(value) =>
        if (value.toBoolean) {
          if (!Environment.isProduction.getOrElse("False").toBoolean) {
            metricsEndPoint = "https://t.cli.privado.ai/api/event?version=2"
          }
          Environment.dockerAccessKey match {
            case Some(dockerKey) =>
              val accessKey = Utilities.getSHA256Hash(dockerKey)
              val requestData = s""" {"event_type": "PRIVADO_CORE",
                                         |  "event_message": ${metricsData.asJson.spaces4},
                                         |  "user_hash": "${Environment.userHash.get}",
                                         |  "session_id": "${Environment.sessionId.get}" }""".stripMargin
              try {
                requests.post(
                  metricsEndPoint,
                  data = requestData,
                  headers = Map("Authentication" -> s"$accessKey", "Content-Type" -> "application/json")
                )
              } catch {
                case e: Exception =>
                  logger.debug("error in uploading metrics to server")
                  logger.debug("The error is ", e)
              }
            case _ => ()
          }
        }
      case _ => ()
    }
  }
}
