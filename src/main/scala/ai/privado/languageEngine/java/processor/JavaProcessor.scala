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

import ai.privado.cache.AppCache
import ai.privado.entrypoint.ScanProcessor.config
import ai.privado.exporter.JSONExporter
import ai.privado.languageEngine.java.passes.config.PropertiesFilePass
import ai.privado.languageEngine.java.semantic.Language._
import ai.privado.metric.MetricHandler
import ai.privado.model.Constants.{outputDirectoryName, outputFileName}
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants}
import ai.privado.semantic.Language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import org.slf4j.LoggerFactory

import java.util.Calendar
import scala.util.{Failure, Success, Try}

object JavaProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    processedRules: ConfigAndRules,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpg) => {
        new PropertiesFilePass(cpg, sourceRepoLocation).createAndApply()

        logger.info("Applying data flow overlay")
        val context = new LayerCreatorContext(cpg)
        val options = new OssDataFlowOptions()
        new OssDataFlow(options).run(context)
        logger.info("=====================")

        // Run tagger
        println(s"${Calendar.getInstance().getTime} - Tagging source code with rules...")
        cpg.runTagger(processedRules)
        println(s"${Calendar.getInstance().getTime} - Finding source to sink flow of data...")
        val dataflowMap = cpg.dataflow

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
    val cpgconfig =
      Config(inputPath = sourceRepoLocation, fetchDependencies = !config.skipDownloadDependencies)
    val xtocpg = JavaSrc2Cpg().createCpgWithOverlays(cpgconfig)
    println(s"${Calendar.getInstance().getTime} - Base processing done...")
    processCPG(xtocpg, processedRules, sourceRepoLocation)
  }

}
