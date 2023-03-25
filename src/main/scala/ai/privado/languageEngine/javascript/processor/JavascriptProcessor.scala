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

import ai.privado.cache.AppCache
import ai.privado.entrypoint.ScanProcessor
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.exporter.JSONExporter
import ai.privado.languageEngine.javascript.passes.methodfullname.{
  MethodFullName,
  MethodFullNameForEmptyNodes,
  MethodFullNameFromIdentifier
}
import ai.privado.languageEngine.javascript.semantic.Language._
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{cpgOutputFileName, outputDirectoryName, outputFileName}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, Language}
import ai.privado.semantic.Language._
import ai.privado.utility.UnresolvedReportUtility
import ai.privado.utility.Utilities.createCpgFolder
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.jssrc2cpg.passes.{ConstClosurePass, JavaScriptTypeHintCallLinker, JavaScriptTypeRecovery, RequirePass}
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.joern.x2cpg.passes.frontend.JavascriptCallLinker
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory

import java.util.Calendar
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Failure, Success, Try}

object JavascriptProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    processedRules: ConfigAndRules,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) =>
        logger.info("Applying default overlays")
        logger.info("Enhancing Javascript graph")
        logger.debug("Running custom passes")
        new MethodFullName(cpg).createAndApply()
        new MethodFullNameFromIdentifier(cpg).createAndApply()
        new MethodFullNameForEmptyNodes(cpg).createAndApply()

        // Unresolved function report
        if (config.showUnresolvedFunctionsReport) {
          val path = s"${config.sourceLocation.head}/${Constants.outputDirectoryName}"
          UnresolvedReportUtility.reportUnresolvedMethods(xtocpg, path, Language.JAVASCRIPT)
        }
        logger.info("=====================")

        // Run tagger
        println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
        cpg.runTagger(processedRules)
        println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
        val dataflowMap = cpg.dataflow(ScanProcessor.config)
        println(s"${Calendar.getInstance().getTime} - No of flows found -> ${dataflowMap.size}")
        println(s"${Calendar.getInstance().getTime} - Brewing result...")
        MetricHandler.setScanStatus(true)
        // Exporting
        JSONExporter.fileExport(cpg, outputFileName, sourceRepoLocation, dataflowMap) match {
          case Left(err) =>
            MetricHandler.otherErrorsOrWarnings.addOne(err)
            Left(err)
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
  def createJavaScriptCpg(
    processedRules: ConfigAndRules,
    sourceRepoLocation: String,
    lang: String
  ): Either[String, Unit] = {

    println(s"${Calendar.getInstance().getTime} - Processing source code using $lang engine")
    println(s"${Calendar.getInstance().getTime} - Parsing source code...")

    val cpgOutputPath = s"$sourceRepoLocation/$outputDirectoryName/$cpgOutputFileName"
    // Create the .privado folder if not present
    createCpgFolder(sourceRepoLocation);

    // Need to convert path to absolute path as javaScriptCpg need abolute path of repo
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath.normalize().toString
    val cpgconfig =
      Config(inputPath = absoluteSourceLocation, outputPath = cpgOutputPath)
    val maybeCpg = new JsSrc2Cpg() createCpgWithOverlays (cpgconfig)
    println("done with base CPG")
    val xtocpg = maybeCpg.map { cpg =>
      new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
      println("done with OssDataFlow")
      new RequirePass(cpg).createAndApply()
      println("done with RequirePass")
      new ConstClosurePass(cpg).createAndApply()
      println("done with ConstClosurePass")
      new JavascriptCallLinker(cpg).createAndApply()
      println("done with JavascriptCallLinker")
//      new JavaScriptTypeRecovery(cpg, enabledDummyTypes = false).createAndApply()
//      println("done with JavaScriptTypeRecovery")
//      new JavaScriptTypeHintCallLinker(cpg).createAndApply()
//      println("done with JavaScriptTypeHintCallLinker")
      //      postProcessingPasses(cpg, Option(config)).foreach(_.createAndApply())
      cpg
    }
    processCPG(xtocpg, processedRules, sourceRepoLocation)
  }

}
