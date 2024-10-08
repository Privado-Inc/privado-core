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

package ai.privado.languageEngine.php.processor

import ai.privado.cache.*
import ai.privado.entrypoint.PrivadoInput
import ai.privado.inputprocessor.DependencyInfo
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.php.semantic.Language.tagger
import ai.privado.model.Constants.*
import ai.privado.model.{CpgWithOutputMap, Language}
import ai.privado.utility.StatsRecorder
import ai.privado.utility.Utilities.createCpgFolder
import io.joern.php2cpg.{Config, Php2Cpg}
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase
import org.slf4j.{Logger, LoggerFactory}

import java.io.File
import java.nio.file.Paths
import java.util.Calendar

class PhpProcessor(
  ruleCache: RuleCache,
  privadoInput: PrivadoInput,
  sourceRepoLocation: String,
  dataFlowCache: DataFlowCache,
  auditCache: AuditCache,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  appCache: AppCache,
  statsRecorder: StatsRecorder,
  returnClosedCpg: Boolean = true,
  databaseDetailsCache: DatabaseDetailsCache = new DatabaseDetailsCache(),
  propertyFilterCache: PropertyFilterCache = new PropertyFilterCache(),
  fileLinkingMetadata: FileLinkingMetadata = new FileLinkingMetadata(),
  dependencies: List[DependencyInfo]
) extends BaseProcessor(
      ruleCache,
      privadoInput,
      sourceRepoLocation,
      Language.PHP,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      statsRecorder,
      returnClosedCpg,
      databaseDetailsCache,
      propertyFilterCache,
      fileLinkingMetadata,
      dependencies
    ) {

  override val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def applyPrivadoPasses(cpg: Cpg): List[CpgPassBase] = super.applyPrivadoPasses(cpg)

  override def runPrivadoTagger(cpg: Cpg, taggerCache: TaggerCache): Unit = {
    super.runPrivadoTagger(cpg, taggerCache)
    cpg.runTagger(
      ruleCache,
      taggerCache,
      privadoInput,
      dataFlowCache,
      appCache,
      databaseDetailsCache,
      statsRecorder,
      fileLinkingMetadata
    )
  }

  override def applyDataflowAndPostProcessingPasses(cpg: Cpg): Unit = {
    super.applyDataflowAndPostProcessingPasses(cpg)
    Php2Cpg.postProcessingPasses(cpg).foreach(_.createAndApply())
  }

  override def processCpg(): Either[String, CpgWithOutputMap] = {
    statsRecorder.justLogMessage("Processing source code using Php engine")
    statsRecorder.initiateNewStage("Base source processing")
    createCpgFolder(sourceRepoLocation)

    // we should have CSV input for the --extensions-for-php flag. eg ".php,.cls"
    val phpExtensions =
      if privadoInput.extensionsForPhp.nonEmpty then privadoInput.extensionsForPhp.split(",").toSet else Set()

    val cpgOutput = Paths.get(sourceRepoLocation, outputDirectoryName, cpgOutputFileName)
    val cpgConfig = Config()
      .withInputPath(sourceRepoLocation)
      .withOutputPath(cpgOutput.toString)
      .withIgnoredFilesRegex(ruleCache.getExclusionRegex)
      .withPhpParserBin(PhpProcessor.parserBinPath)
      .withDownloadDependencies(!privadoInput.skipDownloadDependencies)
      .withExtensions(phpExtensions)

    val xtocpg = new Php2Cpg().createCpg(cpgConfig).map { cpg =>
      statsRecorder.endLastStage()
      statsRecorder.initiateNewStage("Default overlays")
      applyDefaultOverlays(cpg)
      statsRecorder.endLastStage()
      cpg
    }

    tagAndExport(xtocpg)
  }
}

object PhpProcessor {
  val parserBinPath: String = {
    val dir        = getClass.getProtectionDomain.getCodeSource.getLocation.toString
    val indexOfLib = dir.lastIndexOf("lib")
    val fixedDir = if (indexOfLib != -1) {
      new File(dir.substring("file:".length, indexOfLib)).toString
    } else {
      val indexOfTarget = dir.lastIndexOf("target")
      if (indexOfTarget != -1) {
        new File(dir.substring("file:".length, indexOfTarget)).toString
      } else {
        "."
      }
    }

    val parserPath = Environment.isProduction match {
      case Some(_) => Paths.get("/home", "privado-core-build", "php-parser", "php-parser.php")
      case None    => Paths.get(fixedDir, "bin", "php-parser", "php-parser.php")
    }

    println(s"${Calendar.getInstance().getTime} - Using PHP logger from $parserPath")
    parserPath.toAbsolutePath.toString

  }
}
