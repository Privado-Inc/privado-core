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

import ai.privado.model.Constants
import ai.privado.model.exporter._
import org.slf4j.LoggerFactory

import java.net.URL
import scala.collection.mutable
import Console.{BLUE, BOLD, CYAN, GREEN, MAGENTA, RED, RESET, WHITE, YELLOW}

object ConsoleExporter {

  private val logger = LoggerFactory.getLogger(getClass)

  private def getDomainFromString(urlString: String) = {
    try {
      val cleanedUrlString = urlString.replaceAll("'", "").replaceAll("\"", "")
      val prefixToReplace  = if (cleanedUrlString.contains("http://")) "http://" else "https://"
      val url              = new URL("https://" + cleanedUrlString.replaceAll(prefixToReplace, "").trim)
      url.getHost.replaceAll("www.", "").replaceAll("\"", "")
    } catch {
      case e: Exception =>
        logger.debug("Exception while getting domain from string : ", e)
        urlString
    }

  }

  def exportConsoleSummary(
    dataflowsOutput: mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]],
    sources: List[SourceModel],
    sinks: List[SinkModel],
    processing: List[SourceProcessingModel],
    collections: List[CollectionModel],
    violationSize: Int
  ): Unit = {
    // SourceId - Name Map
    val sourceNameIdMap = sources.map((source) => (source.id, source.name)).toMap
    val sinkNameIdMap   = sinks.map((sink) => (sink.id, sink.name)).toMap

    // Leakage Number - SourceId Map
    val leakageSourceMap = dataflowsOutput
      .getOrElse(Constants.leakages, List[DataFlowSubCategoryModel]())
      .map((leakage) => (leakage.sourceId, leakage.sinks.map(_.paths.size).sum))
      .toMap

    // Processing Number - SourceId Map
    val processSourceMap = processing.map(process => (process.sourceId -> process.occurrences.size)).toMap

    // Collections Names - SourceId Map
    val collectionsSourceMap = mutable.HashMap[String, mutable.Set[String]]()
    collections.foreach(collection => {
      collection.collections.foreach(collect => {
        collect.occurrences.foreach(occur => {
          if (collectionsSourceMap.contains(collect.sourceId)) {
            collectionsSourceMap(collect.sourceId).addOne(occur.endPoint)
          } else {
            collectionsSourceMap.addOne(collect.sourceId -> mutable.Set[String](occur.endPoint))
          }
        })
      })
    })

    // Storages - SourceId Map
    val storageSourceMap = dataflowsOutput
      .getOrElse(Constants.storages, List[DataFlowSubCategoryModel]())
      .map(storage =>
        (
          storage.sourceId,
          storage.sinks
            .map(sink => {
              if (sink.databaseDetails.dbName.isBlank)
                sink.name
              else {
                sink.databaseDetails.dbVendor
                  .toUpperCase() + " » " + sink.databaseDetails.dbName + " » " + sink.databaseDetails.dbOperation
                  .toUpperCase()
              }
            })
            .toSet
        )
      )
      .toMap
    val uniqueStorages = storageSourceMap.values.flatten.toSet

    // Third Parties - SourceId Map
    val thirdPartySourceMap = dataflowsOutput
      .getOrElse(Constants.third_parties, List[DataFlowSubCategoryModel]())
      .map(thirdParty => {
        val thirdParties = mutable.Set[String]()
        thirdParty.sinks.foreach(sink => {
          if (sink.apiUrl.nonEmpty) {
            sink.apiUrl.foreach(urlString => {
              thirdParties.addOne(getDomainFromString(urlString))
            })
          } else {
            thirdParties.addOne(sink.name)
          }
        })
        (thirdParty.sourceId, thirdParties)
      })
      .toMap
    val uniqueThirdParties = thirdPartySourceMap.values.flatten.toSet

    // Internal APIs - SourceId Map
    val internalAPIsSourceMap = dataflowsOutput
      .getOrElse(Constants.internal_apis, List[DataFlowSubCategoryModel]())
      .map(internalAPI => {
        val internalAPIs = mutable.Set[String]()
        internalAPI.sinks.foreach(sink => {
          if (sink.apiUrl.nonEmpty) {
            sink.apiUrl.foreach(urlString => {
              internalAPIs.addOne(getDomainFromString(urlString))
            })
          }
        })
        (internalAPI.sourceId, internalAPIs)
      })
      .toMap

    println("\n-----------------------------------------------------------")
    println("SUMMARY")
    println("-----------------------------------------------------------")
    println("\nPrivado discovers data elements that are being collected, processed, or shared in the code.\n")
    println(s"DATA ELEMENTS  |  ${sourceNameIdMap.size} |")
    println(s"THIRD PARTY    |  ${uniqueThirdParties.size} |")
    println(s"STORAGES       |  ${uniqueStorages.size} |")
    println(s"ISSUES         |  ${violationSize} |")
    println("\n---------------------------------------------------------")
    if (sourceNameIdMap.nonEmpty) {
      println(s"${sourceNameIdMap.size} DATA ELEMENTS")
      println(
        "Here is a list of data elements discovered in the code along with details on data flows to third parties, databases and leakages to logs."
      )
    }

    var count = 0;
    sourceNameIdMap.foreachEntry((sourceId, sourceName) => {
      count = count + 1

      Console.println(s"\n${RESET}${WHITE}${BOLD}${count}. ${sourceName.toUpperCase()}${RESET}")

      if (thirdPartySourceMap.contains(sourceId)) {
        Console.println(
          s"\t${RESET}${YELLOW}Sharing${RESET}         ->  ${thirdPartySourceMap(sourceId).toList.mkString(", ")}"
        )
      }
      if (storageSourceMap.contains(sourceId)) {
        Console.println(
          s"\t${RESET}${BLUE}Storage${RESET}         ->  ${storageSourceMap(sourceId).toList.mkString(", ")}"
        )
      }
      if (internalAPIsSourceMap.contains(sourceId)) {
        Console.println(
          s"\t${RESET}${CYAN}Internal API${RESET}    ->  ${internalAPIsSourceMap(sourceId).toList.mkString(", ")}"
        )
      }
      if (leakageSourceMap.contains(sourceId)) {
        Console.println(s"\t${RESET}${RED}Leakage${RESET}         ->  ${leakageSourceMap(sourceId)}")
      }
      if (collectionsSourceMap.contains(sourceId)) {
        Console.println(
          s"\t${RESET}${GREEN}Collections${RESET}     ->  ${collectionsSourceMap(sourceId).toList.mkString(", ")}"
        )
      }
      if (processSourceMap.contains(sourceId)) {
        Console.println(s"\t${RESET}${MAGENTA}Processing${RESET}      ->  ${processSourceMap(sourceId)}")
      }
    })

    println("")
    if (sinkNameIdMap.nonEmpty) {
      println(s"${sinkNameIdMap.size} SINKS")
      println("Here is a list of sinks discovered in the code.")
    }
    count = 0
    sinkNameIdMap.foreachEntry((sinkId, sinkName) => {
      count = count + 1
      Console.println(s"\n${RESET}${WHITE}${BOLD}${count}. ${sinkName.toUpperCase()}${RESET}")
    })
    println("")
  }

}
