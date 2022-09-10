package ai.privado.exporter

import ai.privado.model.exporter.{CollectionModel, DataFlowSubCategoryModel, SourceModel, SourceProcessingModel}

import java.net.URL
import scala.collection.mutable
import Console.{BLUE, BOLD, CYAN, GREEN, MAGENTA, RED, RESET, WHITE, YELLOW}

object ConsoleExporter {

  def exportConsoleSummary(
    dataflowsOutput: mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]],
    sources: List[SourceModel],
    processing: List[SourceProcessingModel],
    collections: List[CollectionModel],
    violationSize: Int
  ): Unit = {
    // Parse the Dataflows
    val sourceNameIdMap = mutable.HashMap[String, String]()
    val leakageSourceMap = mutable.HashMap[String, Int]()
    val processSourceMap = mutable.HashMap[String, Int]()
    val collectionsSourceMap = mutable.HashMap[String, mutable.Set[String]]()
    val storageSourceMap = mutable.HashMap[String, mutable.Set[String]]()
    val thirdPartySourceMap = mutable.HashMap[String, mutable.Set[String]]()
    val internalAPIsSourceMap = mutable.HashMap[String, mutable.Set[String]]()
    val uniqueThirdParties = mutable.Set[String]()

    // SourceId - Name Map
    sources.foreach(source => {
      sourceNameIdMap.addOne(
        source.id.replaceAll("\"", "") -> source.name.replaceAll("\"", "")
      )
    })

    // Leakage Number - SourceId Map
    dataflowsOutput("leakages").foreach(leakage => {
      leakageSourceMap.addOne(leakage.sourceId -> leakage.sinks.size)
    })

    // Processing Number - SourceId Map
    processing.foreach(process => {
      processSourceMap.addOne(process.sourceId -> process.occurrences.size)
    })

    // Collections Names - SourceId Map
    collections.foreach(collection => {
      collection.collections.foreach(collect => {
        if (collectionsSourceMap.contains(collect.sourceId)) {
          collectionsSourceMap(collect.sourceId).addOne(collection.name)
        } else {
          collectionsSourceMap.addOne(collect.sourceId -> mutable.Set[String](collection.name))
        }
      })
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

    println("\n-----------------------------------------------------------")
    println("SUMMARY")
    println("-----------------------------------------------------------")
    println("\nPrivado discovers data elements that are being collected, processed, or shared in the code.\n")
    println(s"DATA ELEMENTS  |  ${sourceNameIdMap.size} |")
    println(s"THIRD PARTY    |  ${uniqueThirdParties.size} |")
    println(s"ISSUES         |  ${violationSize} |")
    println("\n---------------------------------------------------------")
    println(s"${sourceNameIdMap.size} DATA ELEMENTS")
    println("Here is a list of data elements discovered in the code along with details on data flows to third parties, databases and leakages to logs.")

    var count = 0;
    sourceNameIdMap.foreachEntry((sourceId, sourceName) => {
      count = count + 1

      Console.println(s"\n${RESET}${WHITE}${BOLD}${count}. ${sourceName.toUpperCase()}${RESET}")

      if (thirdPartySourceMap.contains(sourceId)) {
        Console.println(s"\t${RESET}${YELLOW}Sharing${RESET}         ->  ${thirdPartySourceMap(sourceId).toList.mkString(", ")}")
      }
      if (storageSourceMap.contains(sourceId)) {
        Console.println(s"\t${RESET}${BLUE}Storage${RESET}         ->  ${storageSourceMap(sourceId).toList.mkString(", ")}")
      }
      if (internalAPIsSourceMap.contains(sourceId)) {
        Console.println(s"\t${RESET}${CYAN}Internal API${RESET}      ->  ${internalAPIsSourceMap(sourceId).toList.mkString(", ")}")
      }
      if (leakageSourceMap.contains(sourceId)) {
        Console.println(s"\t${RESET}${RED}Leakage${RESET}         ->  ${leakageSourceMap(sourceId)}")
      }
      if (collectionsSourceMap.contains(sourceId)) {
        Console.println(s"\t${RESET}${GREEN}Collections${RESET}     ->  ${collectionsSourceMap(sourceId).toList.mkString(", ")}")
      }
      if (processSourceMap.contains(sourceId)) {
        Console.println(s"\t${RESET}${MAGENTA}Processing${RESET}      ->  ${processSourceMap(sourceId)}")
      }
    })

    println("")

  }

}
