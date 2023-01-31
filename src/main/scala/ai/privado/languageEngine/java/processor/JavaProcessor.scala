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

package ai.privado.languageEngine.java.processor

import ai.privado.cache.{AppCache, DataFlowCache}
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.entrypoint.TimeMetric
import ai.privado.exporter.JSONExporter
import ai.privado.languageEngine.java.passes.config.PropertiesFilePass
import ai.privado.languageEngine.java.passes.methodFullName.LoggerLombokPass
import ai.privado.languageEngine.java.semantic.Language._
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{outputDirectoryName, outputFileName, storages}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants}
import ai.privado.semantic.Language._
import better.files.File
import com.google.gson.Gson
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory

import java.util.Calendar
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object JavaProcessor {

  private val logger    = LoggerFactory.getLogger(getClass)
  private var cpgconfig = Config()
  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    processedRules: ConfigAndRules,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) => {
        try {
          println(s"${Calendar.getInstance().getTime} - Processing property files pass")
          new PropertiesFilePass(cpg, sourceRepoLocation).createAndApply()
          println(
            s"${TimeMetric.getNewTime()} - Property file pass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )
          logger.info("Applying data flow overlay")
          val context = new LayerCreatorContext(cpg)
          val options = new OssDataFlowOptions()
          new OssDataFlow(options).run(context)
          logger.info("=====================")
          println(
            s"${TimeMetric.getNewTime()} - Run oss data flow is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )

          // Run tagger
          println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
          cpg.runTagger(processedRules)
          println(
            s"${TimeMetric.getNewTime()} - Tagging source code is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
          )
          println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
          val dataflowMap = cpg.dataflow
          println(s"${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
              .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${DataFlowCache.finalDataflow.size}")
          println(
            s"\n\n${TimeMetric.getNewTime()} - Code scanning is done in \t\t\t- ${TimeMetric.getTheTotalTime()}\n\n"
          )
          println(s"${Calendar.getInstance().getTime} - Brewing result...")
          MetricHandler.setScanStatus(true)
          // Exporting
          JSONExporter.fileExport(cpg, outputFileName, sourceRepoLocation, dataflowMap) match {
            case Left(err) =>
              MetricHandler.otherErrorsOrWarnings.addOne(err)
              Left(err)
            case Right(_) =>
              println(
                s"${Calendar.getInstance().getTime} - Successfully exported output to '${AppCache.localScanPath}/$outputDirectoryName' folder..."
              )
              logger.debug(
                s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
              )
              Right(())
          }
        } finally {
          cpg.close()
          import java.io.File
          val cpgFile = new File(cpgconfig.outputPath)
          println(s"\n\n\nBinary file size -- ${cpgFile.length()} in Bytes - ${cpgFile.length() * 0.000001} MB\n\n\n")
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

  /** Create cpg using Java Language
    *
    * @param sourceRepoLocation
    * @param lang
    * @return
    */
  def createJavaCpg(processedRules: ConfigAndRules, sourceRepoLocation: String, lang: String): Either[String, Unit] = {
    println(s"${Calendar.getInstance().getTime} - Processing source code using ${Languages.JAVASRC} engine")
    if (!config.skipDownloadDependencies)
      println(s"${Calendar.getInstance().getTime} - Downloading dependencies and Parsing source code...")
    else
      println(s"${Calendar.getInstance().getTime} - Parsing source code...")
    cpgconfig = Config(inputPath = sourceRepoLocation, fetchDependencies = !config.skipDownloadDependencies)
    val javasrc = JavaSrc2Cpg()
    val xtocpg = javasrc.createCpg(cpgconfig).map { cpg =>
      println(
        s"${TimeMetric.getNewTime()} - Base processing done in \t\t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      println(s"${Calendar.getInstance().getTime} - Processing Logger Lombok pass")
      new LoggerLombokPass(cpg).createAndApply()
      println(
        s"${TimeMetric.getNewTime()} - Logger Lombok pass done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
      )
      applyDefaultOverlays(cpg)
      cpg
    }
    if (config.showUnresolvedFunctionsReport)
      reportUnresolvedMethods(xtocpg)
    processCPG(xtocpg, processedRules, sourceRepoLocation)
  }

  def reportUnresolvedMethods(xtocpg: Try[Cpg]): Unit = {
    var total = 0
    var unresolvedSignatures = 0
    var unresolvedNamespaces = 0
    var unresolvedSignaturesList = List[String]()
    var unresolvedNamespacesList = List[String]()

    xtocpg match {
      case Success(cpg) => {
        total =  cpg.call.methodFullName.l.length
        unresolvedSignatures = cpg.call.methodFullName("(?i)(.*)(unresolved)(signature)(.*)").l.length
        unresolvedSignaturesList = cpg.call.methodFullName("(?i)(.*)(unresolved)(signature)(.*)").methodFullName.l
        unresolvedNamespaces = cpg.call.methodFullName("(?i)(.*)(unresolved)(namespace)(.*)").l.length
        unresolvedNamespacesList = cpg.call.methodFullName("(?i)(.*)(unresolved)(namespace)(.*)").methodFullName.l
      }
    }

    val statfile = File("java.txt")
    statfile.write("")
    val divider = "---------------------------------------------------------------------------------------------------------"

    println()
    println(divider)
    var statstr = "Total number of function calls: " + total
    println(statstr)
    statfile.appendLine(statstr)
    println()

    var percentage = (unresolvedSignatures * 100) / total
    statstr = "Calls with unresolved signatures: " + unresolvedSignatures + " | " + percentage + "% of total calls"
    println(statstr)
    statfile.appendLine(statstr)

    percentage = (unresolvedNamespaces * 100) / total
    val subsetPercentage  = (unresolvedNamespaces * 100) / unresolvedSignatures
    statstr = "Calls with unresolved namespace: " + unresolvedNamespaces + " | " + percentage + "% of total calls" + " | " + subsetPercentage + "% of unresolved calls"
    println(statstr)
    statfile.appendLine(statstr)

    val resolved = total-unresolvedSignatures
    percentage = (resolved * 100) / total
    println()
    statstr = "Resolved function calls: " + resolved + " | " + percentage + "% of total calls"
    println(statstr)
    statfile.appendLine(statstr)

    statfile.appendLine(divider)
    statfile.appendLine("List of Calls with Unresolved Signatures:")
    unresolvedSignaturesList.zipWithIndex.map { case (us, index) => statfile.appendLine((index+1)+" - "+us) }

    statfile.appendLine(divider)
    statfile.appendLine("List of Calls with Unresolved Namespaces:")
    unresolvedNamespacesList.zipWithIndex.map { case (un, index) => statfile.appendLine((index+1)+" - "+un) }

    println(divider)
    println()
  }
}
