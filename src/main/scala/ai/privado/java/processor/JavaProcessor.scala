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

package ai.privado.java.processor

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.ScanProcessor.{config, logger}
import ai.privado.java.exporter.JSONExporter
import ai.privado.java.passes.config.PropertiesFilePass
import ai.privado.java.semantic.Language._
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, Language}
import ai.privado.model.Constants.{outputDirectoryName, outputFileName}
import ai.privado.utility.Utilities.filterRuleByLanguage
import io.circe.Json
import io.joern.javasrc2cpg.{Config, JavaSrc2Cpg}
import io.joern.joerncli.DefaultOverlays
import io.shiftleft.codepropertygraph
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

import scala.util.{Failure, Success, Try}

object JavaProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    processedRules: ConfigAndRules,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpgWithoutDataflow) => {
        new PropertiesFilePass(cpgWithoutDataflow, sourceRepoLocation).createAndApply()
        logger.info("Applying default overlays")
        cpgWithoutDataflow.close()
        val cpg = DefaultOverlays.create("cpg.bin")
        logger.info("=====================")

        // Run tagger
        println("Tagging source code with rules...")
        cpg.runTagger(processedRules)
        println("Finding source to sink flow of data...")
        val dataflowMap = cpg.dataflow

        println("Brewing result...")
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

    println(s"Processing source code using ${Languages.JAVASRC} engine")
    if (!config.skipDownloadDependencies)
      println("Downloading dependencies and Parsing source code...")
    else
      println("Parsing source code...")
    val cpgconfig =
      Config(inputPath = sourceRepoLocation, fetchDependencies = !config.skipDownloadDependencies)
    val xtocpg = JavaSrc2Cpg().createCpg(cpgconfig)
    processCPG(xtocpg, processedRules, sourceRepoLocation)
  }

}
