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

package ai.privado.javascript.processor

import ai.privado.cache.AppCache
import ai.privado.java.exporter.JSONExporter
import ai.privado.javascript.passes.methodfullname.MethodFullName
import ai.privado.javascript.semantic.Language.tagger
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants}
import ai.privado.model.Constants.{outputDirectoryName, outputFileName}
import io.joern.dataflowengineoss.language.Path
import io.joern.joerncli.DefaultOverlays
import io.joern.jssrc2cpg.{Config, JsSrc2Cpg}
import io.shiftleft.codepropertygraph
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._
import better.files.File

import scala.util.{Failure, Success, Try}

object JavascriptProcessor {

  private val logger = LoggerFactory.getLogger(getClass)

  private def processCPG(
    xtocpg: Try[codepropertygraph.Cpg],
    processedRules: ConfigAndRules,
    sourceRepoLocation: String
  ): Either[String, Unit] = {
    xtocpg match {
      case Success(cpgWithoutDataflow) => {
        logger.info("Applying default overlays")
        cpgWithoutDataflow.close()
        val cpg = DefaultOverlays.create("cpg.bin")
        logger.info("Running custom passes")
        new MethodFullName(cpg).createAndApply()
        logger.info("=====================")

        // Run tagger
        println("Tagging source code with rules...")
        cpg.runTagger(processedRules)
        println("Finding source to sink flow of data...")
        val dataflowMap = Map[String, Path]()
        // cpg.dataflow

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
            val codelist = cpg.call
              .where(
                _.methodFullName
                  .filterNot(_.matches(".*operator.*"))
              )
              .map(item => (item.methodFullName, item.location.filename))
              .dedup
              .l
            println(s"size of code : ${codelist.size}")
            codelist.foreach(println)
            println("Above we printed methodFullName")
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

    println(s"Processing source code using $lang engine")
    println("Parsing source code...")

    // Need to convert path to absolute path as javaScriptCpg need abolute path of repo
    val absoluteSourceLocation = File(sourceRepoLocation).path.toAbsolutePath.normalize().toString
    val cpgconfig =
      Config(inputPath = absoluteSourceLocation)
    val xtocpg = new JsSrc2Cpg().createCpg(cpgconfig)
    processCPG(xtocpg, processedRules, sourceRepoLocation)
  }

}
