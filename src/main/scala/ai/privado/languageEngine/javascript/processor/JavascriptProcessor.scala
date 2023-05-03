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

package ai.privado.languageEngine.javascript.processor

import ai.privado.audit.AuditReportEntryPoint
import ai.privado.cache.{AppCache, DataFlowCache, RuleCache, TaggerCache}
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.entrypoint.{ScanProcessor, TimeMetric}
import ai.privado.exporter.{ExcelExporter, JSONExporter}
import io.joern.jssrc2cpg.passes.{ImportsPass, JavaScriptTypeHintCallLinker, JavaScriptTypeRecoveryPass}
import io.joern.pysrc2cpg.PythonNaiveCallLinker
import ai.privado.languageEngine.javascript.semantic.Language._
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants._
import ai.privado.model.{CatLevelOne, Constants, Language}
import ai.privado.passes.{HTMLParserPass, SQLParser}
import ai.privado.semantic.Language._
import ai.privado.utility.UnresolvedReportUtility
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph
import io.joern.x2cpg.X2Cpg
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.layers.LayerCreatorContext

import java.util.Calendar
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

object JavascriptProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    ruleCache: RuleCache,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) =>
        // Apply default overlays
        X2Cpg.applyDefaultOverlays(cpg)
        new ImportsPass(cpg).createAndApply()

        logger.info("Applying data flow overlay")
        val context = new LayerCreatorContext(cpg)
        val options = new OssDataFlowOptions()
        new OssDataFlow(options).run(context)
        logger.info("=====================")
        println(
          s"${TimeMetric.getNewTime()} - Run oss data flow is done in \t\t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
        )

        new JavaScriptTypeRecoveryPass(cpg).createAndApply()
        println(
          s"${TimeMetric.getNewTime()} - Run JavascriptTypeRecovery done in \t\t- ${TimeMetric.setNewTimeToLastAndGetTimeDiff()}"
        )
        new JavaScriptTypeHintCallLinker(cpg).createAndApply()
        new PythonNaiveCallLinker(cpg).createAndApply()

        new HTMLParserPass(cpg, sourceRepoLocation, ruleCache).createAndApply()

        new SQLParser(cpg, sourceRepoLocation, ruleCache).createAndApply()

        // Unresolved function report
        if (config.showUnresolvedFunctionsReport) {
          val path = s"${config.sourceLocation.head}/${Constants.outputDirectoryName}"
          UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.JAVASCRIPT)
        }
        logger.info("=====================")

        // Run tagger
        println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
        val taggerCache = new TaggerCache
        cpg.runTagger(ruleCache, taggerCache)
        println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
        val dataflowMap = cpg.dataflow(ScanProcessor.config, ruleCache)
        println(s"\n${TimeMetric.getNewTime()} - Finding source to sink flow is done in \t\t- ${TimeMetric
            .setNewTimeToLastAndGetTimeDiff()} - Processed final flows - ${DataFlowCache.finalDataflow.size}")
        println(s"\n${TimeMetric.getNewTime()} - Code scanning is done in \t\t\t- ${TimeMetric.getTheTotalTime()}\n")
        println(s"${Calendar.getInstance().getTime} - Brewing result...")
        MetricHandler.setScanStatus(true)
        val errorMsg = new ListBuffer[String]()
        // Exporting
        JSONExporter.fileExport(cpg, outputFileName, sourceRepoLocation, dataflowMap, ruleCache) match {
          case Left(err) =>
            MetricHandler.otherErrorsOrWarnings.addOne(err)
            errorMsg += err
          case Right(_) =>
            println(s"Successfully exported output to '${AppCache.localScanPath}/$outputDirectoryName' folder")
            logger.debug(
              s"Total Sinks identified : ${cpg.tag.where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).call.tag.nameExact(Constants.id).value.toSet}"
            )
            val codelist = cpg.call
              .whereNot(_.methodFullName(Operators.ALL.asScala.toSeq: _*))
              .map(item => (item.methodFullName, item.location.filename))
              .dedup
              .l
            logger.debug(s"size of code : ${codelist.size}")
            codelist.foreach(item => logger.debug(item._1, item._2))
            logger.debug("Above we printed methodFullName")

            Right(())
        }

        // Exporting the Audit report
        if (ScanProcessor.config.generateAuditReport) {
          ExcelExporter.auditExport(
            outputAuditFileName,
            AuditReportEntryPoint.getAuditWorkbook(),
            sourceRepoLocation
          ) match {
            case Left(err) =>
              MetricHandler.otherErrorsOrWarnings.addOne(err)
              errorMsg += err
            case Right(_) =>
              println(
                s"${Calendar.getInstance().getTime} - Successfully exported Audit report to '${AppCache.localScanPath}/$outputDirectoryName' folder..."
              )
          }
        }

        // Exporting the Intermediate report
        if (ScanProcessor.config.testOutput || ScanProcessor.config.generateAuditReport) {
          JSONExporter.IntermediateFileExport(
            outputIntermediateFileName,
            sourceRepoLocation,
            DataFlowCache.getIntermediateDataFlow()
          ) match {
            case Left(err) =>
              MetricHandler.otherErrorsOrWarnings.addOne(err)
              errorMsg += err
            case Right(_) =>
              println(
                s"${Calendar.getInstance().getTime} - Successfully exported intermediate output to '${AppCache.localScanPath}/${Constants.outputDirectoryName}' folder..."
              )
          }
        }

        // Check if any of the export failed
        if (errorMsg.toList.isEmpty)
          Right(())
        else
          Left(errorMsg.toList.mkString("\n"))

      case Failure(exception) =>
        logger.error("Error while parsing the source code!")
        logger.debug("Error : ", exception)
        MetricHandler.setScanStatus(false)
        Left("Error while parsing the source code: " + exception.toString)
    }
  }

  /** Create cpg using Javascript Language
    *
    * @param sourceRepoLocation
    * @param lang
    * @return
    */
  def createJavaScriptCpg(ruleCache: RuleCache, sourceRepoLocation: String, lang: String): Either[String, Unit] = {

    println(s"${Calendar.getInstance().getTime} - Processing source code using $lang engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    // Need to convert path to absolute path as javaScriptCpg need abolute path of repo
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath.normalize().toString
    val cpgconfig =
      Config(inputPath = absoluteSourceLocation, outputPath = cpgOutputPath)
    val xtocpg = new JsSrc2Cpg().createCpgWithAllOverlays(cpgconfig)
    processCPG(xtocpg, ruleCache, sourceRepoLocation)
  }

}
